module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Alt ((<|>))
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Exception (try)
import Control.Monad.Eff.Ref (modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import Control.Monad.IO (INFINITY, IO, launchIO)
import Control.Monad.IOSync (IOSync, runIOSync')
import Data.Either (Either(Right, Left), fromRight)
import Data.Foreign (F)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Node.ChildProcess (defaultSpawnOptions, onError, onExit, spawn, stdout, toStandardError)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)
import TelegramBot (Bot, Message(..), TELEGRAM, connect, onText, sendMessage)
import TelegramBot as TB

newtype FilePath = FilePath String
derive instance ntFP :: Newtype FilePath _
derive newtype instance rfFP :: ReadForeign FilePath

newtype Token = Token String
derive instance ntT :: Newtype Token _
derive newtype instance rfT :: ReadForeign Token

newtype Id = Id Int
derive instance ntI :: Newtype Id _
derive newtype instance rfI :: ReadForeign Id

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

getConfig :: IO (F Config)
getConfig = liftAff $ readJSON <$> readTextFile UTF8 "./config.json"

runTorscraper :: FilePath -> Request -> IO Result
runTorscraper path request =
  runProgram
    "node"
    ["index.js"]
    (unwrap path)
    { id: request.id, origin: request.origin, output: _ }

runProgram :: forall a. String -> Array String -> String -> (String -> a) -> IO a
runProgram cmd args path format = liftAff $ makeAff \e s -> do
  ref <- newRef ""
  process <- spawn cmd args $
    defaultSpawnOptions { cwd = Just path }
  result <- try $ onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ (_ <> string)
  case result of
    Right _ -> do
      onError process $ toStandardError >>> e
      onExit process \exit -> do
        output <- readRef ref
        s $ format output
    Left err -> e err

getMessages :: Bot -> IOSync (Event Int)
getMessages bot = liftEff do
  let pattern = unsafePartial $ fromRight $ regex "^get$" ignoreCase
  { event, push } <- create
  onText bot pattern $ handler push
  pure event
  where
    handler push m _
      | Right message <- runExcept m
      , Message {from} <- message
      , Just user <- unwrap from
      , TB.User {id} <- user
        = push id
      | otherwise
        = pure unit

connect' :: String -> IOSync Bot
connect' = liftEff <<< connect

subscribe' :: forall a e. Event a -> (a -> Eff (frp :: FRP | e) Unit) -> IOSync Unit
subscribe' a b = liftEff $ subscribe a b

sendMessage' :: forall e. Bot -> Result -> Eff (telegram :: TELEGRAM | e) Unit
sendMessage' connection {id, output} = do
  sendMessage connection (unwrap id) output

type Main
   = { torscraper :: Event Result
     , bot :: Event Request
     , timer :: Event Request
     }
  -> { torscraper :: Event Request
     , bot :: Event Result
     , timer :: Event Unit
     }
main' :: Main
main' sources =
  { torscraper: sources.timer <|> sources.bot
  , bot: sources.torscraper
  , timer: mempty
  }

type Drivers e =
  { torscraper :: Event Request -> Eff e (Event Result)
  , bot :: Event Result -> Eff e (Event Request)
  , timer :: Event Unit -> Eff e (Event Request)
  }

drivers :: forall e. Config -> Drivers (infinity :: INFINITY | e)
drivers
  { token
  , torscraperPath
  , master
  } =
  { torscraper
  , bot
  , timer
  }
  where
    torscraper requests = runIOSync' do
      { event, push } <- liftEff $ create
      subscribe' requests $ handleTorscraper push
      pure event
    handleTorscraper push {origin} = runIOSync' $ launchIO do
      result <- runTorscraper torscraperPath {origin, id: master}
      liftEff $ push result

    bot results = runIOSync' do
      connection <- connect' $ unwrap token
      subscribe' results $ sendMessage' connection
      messages <- getMessages connection
      pure $ { origin: User, id: master } <$ messages

    timer _
      | tick <- pure 0 <|> interval (60 * 60 * 1000)
      , reqs <- { origin: Timer, id: master } <$ tick
      = pure reqs

main :: IOSync Unit
main = launchIO do
  c <- runExcept <$> getConfig
  case c of
    Left e ->
      liftAff <<< AffC.log $ "config.json is malformed: " <> show e
    Right config -> liftEff do
      EffC.log "running"
      runChocoPie main' (drivers config)
