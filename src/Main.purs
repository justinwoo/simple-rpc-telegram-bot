module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Cycle (runRecord)
import Control.Monad.Aff (Aff, attempt, launchAff_, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import Control.XStream (Stream, addListener, create, defaultListener, periodic)
import Data.Either (Either(Right, Left), fromRight)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (insert)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase)
import Node.ChildProcess (CHILD_PROCESS, defaultSpawnOptions, onError, onExit, spawn, stdout, toStandardError)
import Node.Encoding (Encoding(UTF8))
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
runProgram cmd args path format = liftAff $ makeAff \cb -> mempty <$ do
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
        cb <<< Right $ format output
    Left err -> cb $ Left err

getMessages :: Bot -> Eff _ (Stream Int)
getMessages bot = do
  create
    { start: \l -> do
        onText bot pattern $ handler l.next
        pure unit
    , stop: const $ pure unit
    }
  where
    pattern = unsafePartial $ fromRight $ regex "^get$" ignoreCase
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
  void $ runAff (const $ pure unit) aff

type Main
   = { torscraper :: Stream Result
     , bot :: Stream Request
     , timer :: Stream Request
     }
  -> { torscraper :: Stream Request
     , bot :: Stream Result
     , timer :: Stream Unit
     }
main' :: Main
main' sources =
  { torscraper: sources.timer <|> sources.bot
  , bot: sources.torscraper
  , timer: mempty
  }

drivers ::
     Config
  -> { torscraper :: Stream Request -> Eff _ (Stream Result)
     , bot :: Stream Result -> Eff _ (Stream Request)
     , timer :: Stream Unit -> Eff _ (Stream Request)
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
      create
        { start: \l -> do
            _ <- addListener (spec l.next) requests
            pure unit
        , stop: const $ pure unit
        }
      where
        spec push = defaultListener
          { next = handleTorscraper torscraperPath master push
          }

    bot results = do
      connection <- connect $ unwrap token
      _ <- addListener (sendMessageSpec connection) results
      messages <- getMessages connection
      pure $ { origin: FromUser, id: master } <$ messages
      where
        sendMessageSpec connection = defaultListener
          { next = sendMessage' connection
          }
    timer _ = do
      tick <- periodic (60 * 60 * 1000)
      pure $ { origin: FromTimer, id: master } <$ (tick <|> pure 0)

main :: Eff _ Unit
main = launchAff_ do
  c <- readJSON <$> readTextFile UTF8 "./config.json"
  case c of
    Left e ->
      AffC.log $ "config.json is malformed: " <> show e
    Right config -> do
      void $ liftEff $ runRecord main' (drivers config)
