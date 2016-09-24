module Main where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Aff (Canceler, Aff, liftEff', launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (message, EXCEPTION)
import Control.Monad.Eff.Ref (readRef, modifyRef, newRef, REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.XStream (periodic, switchMapEff, STREAM, addListener, fromAff, fromCallback)
import Data.Either (Either(Right, Left), fromRight)
import Data.Foreign (ForeignError, parseJSON)
import Data.Foreign.Class (readProp)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(Just))
import Data.String (indexOf)
import Node.ChildProcess (CHILD_PROCESS, onExit, toStandardError, onError, stdout, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)

type FilePath = String

type Token = String

type Id = Int

type Config =
  { token :: Token
  , torscraperPath :: FilePath
  , master :: Id
  }

data RequestOrigin
  = User
  | Timer

type Request =
  { origin :: RequestOrigin
  , id :: Id
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

type TelegramEffects e = (telegram :: TELEGRAM, console :: CONSOLE | e)

foreign import data TELEGRAM :: !

foreign import data Bot :: *

parseConfig :: String -> Either ForeignError Config
parseConfig json = do
  value <- parseJSON json
  token <- readProp "token" value
  torscraperPath <- readProp "torscraperPath" value
  master <- readProp "master" value
  pure
    { token: token
    , torscraperPath: torscraperPath
    , master: master
    }

getConfig :: forall e. Aff (fs :: FS | e) (Either ForeignError Config)
getConfig = parseConfig <$> readTextFile UTF8 "./config.json"

foreign import connect :: forall e. Token -> Eff (TelegramEffects e) Bot

foreign import _sendMessage :: forall e.
  Fn3
    Bot
    Int
    String
    (Eff (TelegramEffects e) Unit)
sendMessage :: forall e. Bot -> Result -> Eff (TelegramEffects e) Unit
sendMessage bot {id, output, origin} = do
  case origin of
    Timer ->
      case indexOf "nothing new to download" output of
        Just _ -> log "timer found nothing"
        _ -> send
    _ -> send
  where
    send = do
      log output
      runFn3 _sendMessage bot id output

foreign import addMessagesListener :: forall e.
  Fn3
    Bot
    RequestOrigin
    (Request -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)

runTorscraper :: forall e.
  String ->
  Request ->
  Aff
    ( ref :: REF
    , err :: EXCEPTION
    , cp :: CHILD_PROCESS
    | e
    )
    Result
runTorscraper path request = makeAff \e s -> do
  ref <- newRef ""
  process <- spawn "node" ["index.js"] $
    defaultSpawnOptions { cwd = Just path }
  onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ append string
  onError process $ toStandardError >>> e
  onExit process \exit -> do
    output <- readRef ref
    s { id: request.id, origin: request.origin, output: output }

liftEff'' :: forall e a. Eff (err :: EXCEPTION | e) a -> Aff e a
liftEff'' = map (unsafePartial fromRight) <$> liftEff'

main :: forall e.
  Eff
    (MyEffects (err :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  config <- getConfig
  case config of
    Left e -> liftEff' $ log "config.json is malformed. closing."
    Right {token, torscraperPath, master} -> do
      bot <- liftEff $ connect token
      requests <- liftEff $ fromCallback $ runFn3 addMessagesListener bot User
      let timerRequest = {id: master, origin: Timer}
      timer <- liftEff $ periodic (60 * 60 * 1000)
      let timer' = const timerRequest <$> timer
      results <- liftEff'' $ (requests <|> timer' <|> pure timerRequest) `switchMapEff` \request ->
        fromAff $ runTorscraper torscraperPath request
      liftEff' $ addListener
        { next: sendMessage bot
        , error: message >>> log
        , complete: const $ pure unit
        }
        results
