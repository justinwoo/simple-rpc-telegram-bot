module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(Right, Left), fromRight)
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), contains)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Simple.JSON (class ReadForeign, readJSON)
import Sunde as Sunde
import TelegramBot (Bot, connect, onText, sendMessage)
import Type.Prelude (SProxy(..))

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
  = FromUser
  | FromTimer

type Request =
  { origin :: RequestOrigin
  , id :: Id
  }

type Result =
  { id :: Id
  , output :: String
  , origin :: RequestOrigin
  }

runTorscraper :: FilePath -> Request -> Aff Result
runTorscraper path request = do
  result <- Sunde.spawn "runghc" ["scrape.hs"]
    CP.defaultSpawnOptions { cwd = Just (unwrap path) }
  pure case result.exit of
    CP.Normally 0 -> mkResult result.stdout
    _ -> mkResult $ "error: " <> result.stderr
  where
    mkResult o =
      Record.insert (SProxy :: SProxy "output") o request

getMessages :: Bot -> Effect (Event Int)
getMessages bot = do
  let pattern = unsafePartial $ fromRight $ regex "^get$" ignoreCase
  { event, push } <- create
  onText bot pattern $ handler push
  pure event
  where
    handler push m _
      | Right message <- runExcept m
      , Just user <- message.from
        = push user.id
      | otherwise
        = pure unit

sendMessage' :: Bot -> Result -> Effect Unit
sendMessage' connection {id, output, origin} =
  case origin of
  FromUser -> do
    log $ "User: " <> output
    sendMessage connection (unwrap id) output
  FromTimer -> do
    if contains (Pattern "nothing new to download") output
       then pure unit
       else sendMessage connection (unwrap id) output
    log $ "Timer: " <> output

handleTorscraper :: FilePath -> Id -> (Result -> Effect Unit) -> Request -> Effect Unit
handleTorscraper torscraperPath master push request@{origin} = launchAff_ do
  result <- attempt $ runTorscraper torscraperPath {origin, id: master}
  liftEffect case result of
    Right x -> push x
    Left e -> push $ Record.insert (SProxy :: SProxy "output") ("error: " <> show e) request

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

drivers
  :: Config
  -> { torscraper :: Event Request -> Effect (Event Result)
     , bot :: Event Result -> Effect (Event Request)
     , timer :: Event Unit -> Effect (Event Request)
     }
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
    torscraper requests = do
      { event, push } <- create
      _ <- subscribe requests $ handleTorscraper torscraperPath master push
      pure event

    bot results = do
      connection <- connect $ unwrap token
      _ <- subscribe results $ sendMessage' connection
      messages <- getMessages connection
      pure $ { origin: FromUser, id: master } <$ messages

    timer _
      | tick <- pure unit <|> unit <$ interval (60 * 60 * 1000)
      , reqs <- { origin: FromTimer, id: master } <$ tick
      = pure reqs

main :: Effect Unit
main = launchAff_ do
  c <- readJSON <$> readTextFile UTF8 "./config.json"
  case c of
    Left e ->
      log $ "config.json is malformed: " <> show e
    Right config -> do
      liftEffect $ runChocoPie main' (drivers config)
