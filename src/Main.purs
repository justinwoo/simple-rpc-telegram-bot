module Main where

import Prelude
import Control.Monad.Aff.Console as AffC
import TelegramBot as TB
import Control.Alt ((<|>))
import Control.Monad.Aff (Canceler, Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (try, message, EXCEPTION)
import Control.Monad.Eff.Ref (readRef, modifyRef, newRef, REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Except (runExcept)
import Control.XStream (switchMapEff, Stream, STREAM, addListener, fromAff, periodic, create)
import Data.Either (fromRight, Either(Right, Left))
import Data.Foreign (F, parseJSON)
import Data.Foreign.Class (read, class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.String (Pattern(Pattern), indexOf)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
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
instance isForeignConfig :: IsForeign Config where
  read x = readGeneric (defaultOptions {unwrapSingleConstructors = true}) x

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
getConfig = (read <=< parseJSON) <$> readTextFile UTF8 "./config.json"

sendMessage' :: forall e.
  Bot ->
  Result ->
  Eff
    ( telegram :: TELEGRAM
    , console :: CONSOLE
    | e
    ) Unit
sendMessage' bot {id, output, origin} = do
  case origin of
    Timer ->
      case indexOf (Pattern "nothing new to download") output of
        Just _ -> log "timer found nothing"
        _ -> send
    _ -> send
  where
    send = do
      log output
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

onText' :: forall e.
  Bot ->
  Eff
    ( stream :: STREAM
    , telegram :: TELEGRAM
    | e
    )
    (Stream Int)
onText' bot = do
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

onMessage' :: forall e.
  Bot ->
  Eff
    ( stream :: STREAM
    , telegram :: TELEGRAM
    | e
    )
    (Stream TB.Location)
onMessage' bot = do
  create
    { start: \l -> do
         onMessage bot \m -> do
           case runExcept m of
             Right (Message {location: NullOrUndefined (Just x)}) -> do
               l.next x
             _ -> pure unit
    , stop: const $ pure unit
    }

main :: forall e.
  Eff
    (MyEffects (err :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  config <- runExcept <$> getConfig
  case config of
    Left e ->
      AffC.log $ "config.json is malformed: " <> show e
    Right config' ->
      liftEff $ setupBot config'
  where
    setupBot (Config {token, torscraperPath, lastTrainHomePath, master}) = do
      bot <- connect token
      requests <- map {id: _, origin: User} <$> onText' bot
      let timerRequest = {id: master, origin: Timer}
      timer <- periodic (60 * 60 * 1000)
      let timer' = const timerRequest <$> timer
      results <- (requests <|> timer' <|> pure timerRequest) `switchMapEff` \request ->
        fromAff $ runTorscraper torscraperPath request
      routeRequests <- map {id: master, origin: User, location: _} <$> onMessage' bot
      routes <- routeRequests `switchMapEff` \x ->
        fromAff $ runLastTrainHome lastTrainHomePath x
      addListener
        { next: sendMessage' bot
        , error: message >>> log
        , complete: const $ pure unit
        }
        (results <|> routes)
