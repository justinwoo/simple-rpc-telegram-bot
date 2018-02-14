module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, attempt, launchAff_, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Right, Left), fromRight)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (insert)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Time (interval)
import Node.ChildProcess (CHILD_PROCESS, defaultSpawnOptions, onError, onExit, spawn, stdout, toStandardError)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Aff (readTextFile)
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readJSON)
import TelegramBot (Bot, TELEGRAM, connect, onText, sendMessage)
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

runTorscraper :: forall e
   . FilePath
  -> Request
  -> Aff
       ( ref :: REF
       , cp :: CHILD_PROCESS
       | e
       )
       Result
runTorscraper path request =
  runProgram
    "node"
    ["index.js"]
    (unwrap path)
    handler
  where
    handler o =
      insert (SProxy :: SProxy "output") o request

runProgram :: forall a e
   . String
   -> Array String
   -> String
   -> (String -> a)
   -> Aff
       ( ref :: REF
       , cp :: CHILD_PROCESS
       | e
       )
       a
runProgram cmd args path format = liftAff $ makeAff \cb -> do
  ref <- newRef ""
  process <- spawn cmd args $
    defaultSpawnOptions { cwd = Just path }
  result <- try $ onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ (_ <> string)
  case result of
    Right _ -> do
      onError process $ toStandardError >>> Left >>> cb
      onExit process \exit -> do
        output <- readRef ref
        cb <<< pure $ format output
    Left err -> cb $ Left err
  pure mempty

getMessages :: forall e
   . Bot
  -> Eff
       ( frp :: FRP
       , telegram :: TELEGRAM
       | e
       )
       (Event Int)
getMessages bot = do
  let pattern = unsafePartial $ fromRight $ regex "^get$" ignoreCase
  { event, push } <- create
  onText bot pattern $ handler push
  pure event
  where
    handler push m _
      | Right message <- runExcept m
      , NullOrUndefined (Just user) <- message.from
        = push user.id
      | otherwise
        = pure unit

sendMessage' :: forall e
   . Bot
   -> Result
   -> Eff
        ( telegram :: TELEGRAM
        | e
        )
        Unit
sendMessage' connection {id, output} = do
  sendMessage connection (unwrap id) output

handleTorscraper :: forall e
   . FilePath
  -> Id
  -> (Result -> Eff (ref :: REF, cp :: CHILD_PROCESS | e) Unit)
  -> Request
  -> Eff
       ( ref :: REF
       , cp :: CHILD_PROCESS
       | e
       )
       Unit
handleTorscraper torscraperPath master push request@{origin} = yoloAff do
  result <- attempt $ runTorscraper torscraperPath {origin, id: master}
  liftEff case result of
    Right x -> push x
    Left e -> push $ insert (SProxy :: SProxy "output") ("error: " <> show e) request

yoloAff :: forall a e. Aff e a -> Eff e Unit
yoloAff aff =
  unit <$ runAff (const $ pure unit) aff

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

drivers :: forall e1 e2 e3
   . Config
  -> { torscraper
         :: Event Request
         -> Eff
              ( frp :: FRP
              , cp :: CHILD_PROCESS
              , ref :: REF
              | e1
              )
              (Event Result)
     , bot
         :: Event Result
         -> Eff
              ( telegram :: TELEGRAM
              , frp :: FRP
              | e2
              )
              (Event Request)
     , timer
         :: Event Unit
         -> Eff e3
              (Event Request)
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
      | tick <- pure 0 <|> interval (60 * 60 * 1000)
      , reqs <- { origin: FromTimer, id: master } <$ tick
      = pure reqs

main :: forall e.
  Eff
    ( fs :: FS
    , console :: CONSOLE
    , frp :: FRP
    , cp :: CHILD_PROCESS
    , ref :: REF
    , telegram :: TELEGRAM
    | e
    )
    Unit
main = launchAff_ do
  c <- readJSON <$> readTextFile UTF8 "./config.json"
  case c of
    Left e ->
      AffC.log $ "config.json is malformed: " <> show e
    Right config -> do
      liftEff $ runChocoPie main' (drivers config)
