module Main where

import Prelude
import Control.Monad.Aff (Canceler, liftEff', makeAff, Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Either (Either)

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
foreign import readTextFile :: forall e. (String -> Eff (fs :: FS | e) Unit) -> String -> Eff (fs :: FS | e) Unit
readTextFile' :: forall e. String -> Aff (fs :: FS | e) String
readTextFile' x = makeAff (\e s -> readTextFile s x)
foreign import parseConfig :: String -> Config
getConfig :: forall e. Aff (fs :: FS | e) Config
getConfig = parseConfig <$> readTextFile' "./config.json"

foreign import data TELEGRAM :: !
type TelegramEffects e = (telegram :: TELEGRAM, console :: CONSOLE | e)
foreign import data Bot :: *
foreign import connect :: forall e.
  (Bot -> Eff (TelegramEffects e) Unit) ->
  Token ->
  Eff (TelegramEffects e) Unit
connect' :: forall e. Token -> Aff (TelegramEffects e) Bot
connect' token = makeAff (\e s -> connect s token)

foreign import sendMessage :: forall e. Bot -> Result -> Eff (TelegramEffects e) Unit

type Request =
  { origin :: String
  , id :: Id
  }

foreign import addMessagesListener :: forall e.
  Bot ->
  (Request -> Eff (TelegramEffects e) Unit) ->
  Eff (TelegramEffects e) Unit

foreign import data TIMER :: !
foreign import interval :: forall e.
  Int ->
  Id ->
  (Request -> Eff (timer :: TIMER | e) Unit) ->
  Eff (timer :: TIMER | e) Unit

type Result =
  { id :: Id
  , output :: String
  , origin :: Origin
  }
foreign import runTorscraper :: forall e.
  FilePath ->
  (Result -> Eff (console :: CONSOLE | e) Unit) ->
  Request ->
  Eff (console :: CONSOLE | e) Unit

subscribeToSource :: forall e a.
  a ->
  (a -> Eff ( err :: EXCEPTION | e ) Unit ) ->
  Aff e (Either Error Unit)
subscribeToSource handler source = liftEff' $ source $ handler

type MyEffects e =
  ( fs :: FS
  , telegram :: TELEGRAM
  , console :: CONSOLE
  , timer :: TIMER
  | e
  )

main :: forall e.
  Eff
    (MyEffects (err :: EXCEPTION | e))
    (Canceler (MyEffects e))
main = launchAff $ do
  {token, torscraperPath, master} <- getConfig
  bot <- connect' token

  let subscribeToSource' = subscribeToSource $ (runTorscraper torscraperPath) (sendMessage bot)

  subscribeToSource' $ addMessagesListener bot
  subscribeToSource' $ interval (60 * 60 * 1000) master
