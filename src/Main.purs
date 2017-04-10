module Main where

import Prelude
import Control.Monad.Aff.Console as AffC
import TelegramBot as TB
import Control.Alt ((<|>))
import Control.Cycle (run)
import Control.Monad.Aff (Canceler, Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (try, message, EXCEPTION)
import Control.Monad.Eff.Ref (readRef, modifyRef, newRef, REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)
import Control.XStream (STREAM, Stream, addListener, create, defaultListener, fromAff, periodic, switchMapEff)
import Data.Either (fromRight, Either(Right, Left))
import Data.Foreign (F)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.String (Pattern(Pattern), indexOf)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
import Data.Tuple (Tuple(..))
import Node.ChildProcess (CHILD_PROCESS, onExit, toStandardError, onError, stdout, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import TelegramBot (onMessage, Message(Message), sendMessage, onText, TELEGRAM, Bot, connect)

type FilePath = String

type Token = String

type Id = Int

newtype Config = Config
  { token :: String
  , torscraperPath :: String
  , lastTrainHomePath :: String
  , master :: Int
  }
derive instance genericConfig :: Generic Config _
instance decodeConfig :: Decode Config where
  decode x = genericDecode (defaultOptions {unwrapSingleConstructors = true}) x

data Query
  = ScrapeRequest
  | TimerRequest
  | LastTrainRequest RequestWithOrigin
  | QueueMessage Result

data Command
  = ScrapeCommand RequestOrigin
  | LastTrainCommand RequestWithOrigin
  | SendMessage Result
  | Info String

data RequestOrigin
  = User
  | Timer

type Request =
  { origin :: RequestOrigin
  , id :: Id
  }

type RequestWithOrigin =
  { origin :: RequestOrigin
  , id :: Id
  , location :: TB.Location
  }

type Result =
  { id :: Id
  , output :: String
  , origin :: RequestOrigin
  }

type MyEffects e =
  ( fs :: FS
  , telegram :: TELEGRAM
  , cp :: CHILD_PROCESS
  , ref :: REF
  , console :: CONSOLE
  , timer :: TIMER
  , stream :: STREAM
  | e
  )

getConfig :: forall e. Aff (fs :: FS | e) (F Config)
getConfig = decodeJSON <$> readTextFile UTF8 "./config.json"

sendMessage' :: forall e.
  Bot ->
  Result ->
  Eff
    ( telegram :: TELEGRAM
    | e
    ) Unit
sendMessage' bot {id, output} = do
  sendMessage bot id output

runTorscraper :: forall e.
  String ->
  Request ->
  Aff
    ( ref :: REF
    , cp :: CHILD_PROCESS
    | e
    )
    Result
runTorscraper path request = makeAff \e s -> do
  ref <- newRef ""
  process <- spawn "node" ["index.js"] $
    defaultSpawnOptions { cwd = Just path }
  result <- try $ onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ append string
  case result of
    Right _ -> do
      onError process $ toStandardError >>> e
      onExit process \exit -> do
        output <- readRef ref
        s { id: request.id, origin: request.origin, output: output }
    Left err -> e err

runLastTrainHome :: forall e.
  String ->
  RequestWithOrigin ->
  Aff
    ( ref :: REF
    , cp :: CHILD_PROCESS
    | e
    )
    Result
runLastTrainHome path request@{location: TB.Location {latitude, longitude}} = makeAff \e s -> do
  ref <- newRef ""
  process <- spawn "last-train-home" ["--lat", show latitude, "--lon", show longitude] $
    defaultSpawnOptions { cwd = Just path }
  result <- try $ onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ append string
  case result of
    Right _ -> do
      onError process $ toStandardError >>> e
      onExit process \exit -> do
        output <- readRef ref
        s { id: request.id, origin: request.origin, output: output }
    Left err -> e err

getMessages :: forall e.
  Bot ->
  Eff
    ( stream :: STREAM
    , telegram :: TELEGRAM
    | e
    )
    (Stream Int)
getMessages bot = do
  let pattern = unsafePartial $ fromRight $ regex "^get$" ignoreCase
  create
    { start: \l -> do
        onText bot pattern \m s -> do
          case runExcept m of
            Right (Message {from: NullOrUndefined (Just (TB.User user))}) -> do
              l.next user.id
            _ -> pure unit
    , stop: const $ pure unit
    }

locationMessages :: forall e.
  Bot ->
  Eff
    ( stream :: STREAM
    , telegram :: TELEGRAM
    , console :: CONSOLE
    | e
    )
    (Stream TB.Location)
locationMessages bot = do
  create
    { start: \l -> do
         onMessage bot \m -> do
           case runExcept m of
             Right (Message {location: NullOrUndefined (Just x)}) -> do
               l.next x
             _ -> pure unit
    , stop: const $ pure unit
    }

main_ :: Stream Query -> Stream Command
main_ queries = inner <$> queries
  where
    inner = case _ of
      TimerRequest -> ScrapeCommand Timer
      ScrapeRequest -> ScrapeCommand User
      LastTrainRequest req -> LastTrainCommand req
      QueueMessage result@{origin, output} -> do
        case Tuple origin (indexOf (Pattern "nothing new to download") output) of
          Tuple Timer (Just _) ->
            Info "timer found nothing"
          _ ->
            SendMessage result

driver :: forall e. Config -> Stream Command -> Eff (MyEffects e) (Stream Query)
driver
  (Config
    { token
    , torscraperPath
    , lastTrainHomePath
    , master
    }
  )
  commands = do
    bot <- connect token
    timer <- periodic (60 * 60 * 1000)
    scrapeRequests <- getMessages bot
    lastTrainRequests <- locationMessages bot

    -- run torscraper
    scrapeResults <- (scrapeCommands <|> pure Timer) `switchMapEff` \origin ->
      fromAff $ runTorscraper torscraperPath {origin, id: master}

    -- run last-train-home
    routes <- routeCommands `switchMapEff` \x ->
      fromAff $ runLastTrainHome lastTrainHomePath x

    -- print out all commands
    addListener defaultListener commands

    -- send messages to my bot with the results of scraping and routes
    addListener
      { next: sendMessage' bot
      , error: message >>> log
      , complete: const $ pure unit
      }
      messages

    -- gather all of my queries
    pure
      $ const TimerRequest <$> timer
      <|> const ScrapeRequest <$> scrapeRequests
      <|> LastTrainRequest <<< {id: master, origin: User, location: _} <$> lastTrainRequests
      <|> QueueMessage <$> (scrapeResults <|> routes)

    where
      scrapeCommands = commands >>= case _ of
        ScrapeCommand origin -> pure origin
        _ -> mempty
      routeCommands = commands >>= case _ of
        LastTrainCommand location -> pure location
        _ -> mempty
      messages = commands >>= case _ of
        SendMessage result -> pure result
        _ -> mempty


main :: forall e.
  Eff
    (MyEffects (exception :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  runExcept <$> getConfig >>=
  case _ of
    Left e ->
      AffC.log $ "config.json is malformed: " <> show e
    Right config ->
      liftEff <<< void $ run main_ (driver config)
