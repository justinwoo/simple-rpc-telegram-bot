module Main where

import Prelude
import Control.Monad.Eff.Console as EffC
import Control.Alt ((<|>))
import Control.Monad.Aff (Canceler(Canceler), Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Observable (OBSERVABLE, subscribe, free, observable)
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
runTorscraper path req = makeAff (\e s -> runFn3 _runTorscraper path req s)

handleRequest :: forall e.
  String ->
  (Result -> Eff (ConsoleEffects e) Unit) ->
  (Request -> Eff (ConsoleEffects (err :: EXCEPTION | e)) Unit)
handleRequest path send request = void <<< launchAff $ do
  result <- runTorscraper path request
  liftEff $ void $ send result

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

-- my old signature:
-- main :: forall e.
--   Eff
--     (MyEffects (err :: EXCEPTION | e))
--     (Canceler (MyEffects e))
--
-- generated, doesn't work due to missing type class instance:
-- main :: forall t136.
--   Eff
--     ( err :: EXCEPTION
--     , fs :: FS
--     , telegram :: TELEGRAM
--     , console :: CONSOLE
--     | t136
--     )
--     (Canceler
--        ( fs :: FS
--        , telegram :: TELEGRAM
--        , console :: CONSOLE
--        | t136
--        )
--     )
main = launchAff $ do
  {token, torscraperPath, master} <- getConfig
  bot <- connect token
  requests <- liftEff $ observable \sink -> do
    runFn2 addMessagesListener bot sink.next
    free []
  timer <- liftEff $ observable \sink -> do
    runFn3 interval (60 * 60 * 1000) master sink.next
    free []
  liftEff $ subscribe
    { next: (sendMessage bot)
    , error: message >>> EffC.log
    , complete: pure unit
    }
    $ (requests <|> timer) >>= \request -> unsafePerformEff $ observable \sink -> do
      runFn3 _runTorscraper torscraperPath request sink.next
      free []
