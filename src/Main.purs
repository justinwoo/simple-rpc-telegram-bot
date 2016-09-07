module Main where

import Prelude
import Control.Monad.Eff.Console as EffC
import Control.Alt ((<|>))
import Control.Monad.Aff (Canceler, Aff, liftEff', launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Observable (Observable, EffO, OBSERVABLE, subscribe, bindEffO)
import Control.Observable.Lift (liftCallback, liftAff)
import Data.Function.Uncurried (Fn3, runFn3, runFn2, Fn2)

type FilePath = String
type Origin = String
type Token = String
type Id = Int
type Config =
  { token :: Token
  , torscraperPath :: FilePath
  , master :: Id
  }

foreign import data FS :: !
foreign import _readTextFile :: forall e.
  Fn2
    String
    (String -> Eff (fs :: FS | e) Unit)
    (Eff (fs :: FS | e) Unit)
readTextFile :: forall e. String -> Aff (fs :: FS | e) String
readTextFile x = makeAff (\e s -> runFn2 _readTextFile x s)
foreign import parseConfig :: String -> Config
getConfig :: forall e. Aff (fs :: FS | e) Config
getConfig = parseConfig <$> readTextFile "./config.json"

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

foreign import data TIMER :: !
foreign import interval :: forall e.
  Fn3
    Int
    Id
    (Request -> Eff (timer :: TIMER | e) Unit)
    (Eff (timer :: TIMER | e) Unit)

type Result =
  { id :: Id
  , output :: String
  , origin :: Origin
  }

foreign import _runTorscraper :: forall e.
  Fn3
    FilePath
    Request
    (Result -> Eff (ConsoleEffects e) Unit)
    (Eff (ConsoleEffects e) Unit)
runTorscraper :: forall e. FilePath -> Request -> Aff (ConsoleEffects e) Result
runTorscraper path request = makeAff (\e s -> runFn3 _runTorscraper path request s)

type ConsoleEffects e =
  ( console :: CONSOLE
  | e
  )

type MyEffects e =
  ( fs :: FS
  , telegram :: TELEGRAM
  , console :: CONSOLE
  , timer :: TIMER
  , observable :: OBSERVABLE
  | e
  )

bindEff :: forall e a b. Observable a -> (a -> EffO e (Observable b)) -> Observable b
bindEff o f = o >>= f >>> unsafePerformEff

main :: forall e.
  Eff
    (MyEffects (err :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  {token, torscraperPath, master} <- getConfig
  bot <- connect token
  requests <- liftEff $ liftCallback $ runFn2 addMessagesListener bot
  timer <- liftEff $ liftCallback $ runFn3 interval (60 * 60 * 1000) master
  results <- liftEff $ (requests <|> timer) `bindEffO` \request ->
    liftAff $ runTorscraper torscraperPath request
  liftEff' $ subscribe
    { next: (sendMessage bot)
    , error: message >>> EffC.log
    , complete: pure unit
    }
    $ results
