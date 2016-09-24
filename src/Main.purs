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
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(Just))
import Node.ChildProcess (CHILD_PROCESS, onExit, toStandardError, onError, stdout, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)

type FilePath = String
type Origin = String
type Token = String
type Id = Int
type Config =
  { token :: Token
  , torscraperPath :: FilePath
  , master :: Id
  }

parseConfig :: String -> Either ForeignError Config
parseConfig json = do
  value <- parseJSON json
  token <- readProp "token" value
  torscraperPath <- readProp "torscraperPath" value
  master <- readProp "master" value
  pure $ { token: token
  , torscraperPath: torscraperPath
  , master: master
  }

getConfig :: forall e. Aff (fs :: FS | e) (Either ForeignError Config)
getConfig = parseConfig <$> readTextFile UTF8 "./config.json"

foreign import data TELEGRAM :: !
type TelegramEffects e = (telegram :: TELEGRAM, console :: CONSOLE | e)
foreign import data Bot :: *
foreign import _connect :: forall e.
  Fn2
    Token
    (Bot -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)
connect :: forall e. Token -> Aff (TelegramEffects e) Bot
connect token = makeAff (\e s -> runFn2 _connect token s)

foreign import _sendMessage :: forall e.
  Fn2
    Bot
    Result
    (Eff (TelegramEffects e) Unit)
sendMessage :: forall e. Bot -> Result -> Eff (TelegramEffects e) Unit
sendMessage bot result = runFn2 _sendMessage bot result

type Request =
  { origin :: String
  , id :: Id
  }

foreign import addMessagesListener :: forall e.
  Fn2
    Bot
    (Request -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)

type Result =
  { id :: Id
  , output :: String
  , origin :: Origin
  }

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

type ConsoleEffects e =
  ( console :: CONSOLE
  | e
  )

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
      let timerRequest = {id: master, origin: "timer"}
      bot <- connect token
      requests <- liftEff $ fromCallback $ runFn2 addMessagesListener bot
      timer <- liftEff'' $ periodic (60 * 60 * 1000)
      let timer' = const timerRequest <$> timer
      results <- liftEff'' $ (requests <|> timer' <|> pure timerRequest) `switchMapEff` \request ->
        fromAff $ runTorscraper torscraperPath request
      liftEff' $ addListener
        { next: sendMessage bot
        , error: message >>> log
        , complete: const $ pure unit
        }
        results
