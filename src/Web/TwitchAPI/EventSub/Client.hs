{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      :  Web.TwitchAPI.EventSub.Client
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.EventSub.Client
    ( App, new, run
    , subscribe
    , fdLog, errLog, nullLog
    ) where

import Prelude

import Control.Monad.Catch    ( MonadMask )
import Control.Monad.IO.Class ( MonadIO )
import Data.Text              ( Text )
import System.IO              ( Handle, hPutStrLn, hPutStr, stderr )

import qualified Data.Aeson              as JSON
import qualified Data.ByteString         as BS
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text ( decodeUtf8 )
import qualified Network.WebSockets      as WS
import qualified Wuss

import qualified Web.TwitchAPI.EventSub.Request      as ESReq
import qualified Web.TwitchAPI.EventSub.WebSockets   as ES
import qualified Web.TwitchAPI.EventSub.WebSockets   as Welcome ( Welcome(..) )
import qualified Web.TwitchAPI.EventSub.Notification as Notification

import qualified Web.TwitchAPI.Helix.EventSub as TES

import qualified Web.TwitchAPI.Helix.Client as HelixClient

type ErrorHandler = String -> Text -> IO ()
type MessageHandler = Notification.Notification -> Text -> IO ()

data App = App { helix :: HelixClient.Client
               , errorHandler :: ErrorHandler
               , messageHandler :: MessageHandler
               , subscriptions :: [ESReq.Subscription]
               }

fdLog :: Handle -> ErrorHandler
fdLog handle err originalMessage =
    hPutStr handle ">> " >> hPutStrLn handle err >> hPutStr handle ">> " >> hPutStrLn handle (Text.unpack originalMessage) >> hPutStrLn handle ""

errLog :: ErrorHandler
errLog = fdLog stderr

nullLog :: ErrorHandler
nullLog = (const . const . return) ()

new :: HelixClient.Client -> ErrorHandler -> MessageHandler -> App
new h e m = App h e m []

subscribe :: App -> ESReq.Subscription -> App
subscribe app sub =
    let subs = subscriptions app
    in app { subscriptions = sub:subs }

decodeMessage :: BS.ByteString -> Either String ES.Message
decodeMessage = JSON.eitherDecode . BS.fromStrict

run :: (MonadIO m, MonadMask m) => App -> m ()
run app = Wuss.runSecureClient "eventsub.wss.twitch.tv" 443 "/ws" (runApp app)

runApp :: App -> WS.ClientApp ()
runApp app conn = do
    m <- WS.receiveData conn
    print m
    case decodeMessage m of
      Left err -> errorHandler app err (Text.decodeUtf8 m)
      Right (ES.WelcomeMessage _ Welcome.Welcome{ Welcome.sessionID }) -> do
          resps <- mapM (batchSubscribe sessionID) (subscriptions app)
          mapM_ (logFailures (Text.decodeUtf8 m)) resps
          runApp' app conn
      _ -> errorHandler app "Unexpected response received from Twitch" $ Text.decodeUtf8 m
  where
    batchSubscribe :: Text -> ESReq.Subscription -> IO (Either String TES.SubscriptionResponse)
    batchSubscribe sessionID a = HelixClient.doRequest (helix app) $ TES.SubscriptionRequest (ESReq.Websocket sessionID) a
    logFailures :: Text -> Either String TES.SubscriptionResponse -> IO ()
    logFailures m (Left s) = errorHandler app s m
    logFailures _ (Right _) = return ()

runApp' :: App -> WS.ClientApp ()
runApp' app conn = do
    m <- WS.receiveData conn
    case decodeMessage m of
        Left err -> errorHandler app err (Text.decodeUtf8 m) >> runApp' app conn
        Right (ES.NotificationMessage  _ message) -> messageHandler app message (Text.decodeUtf8 m) >> runApp' app conn
        Right _ -> errorHandler app "Unhandled message" (Text.decodeUtf8 m) >> runApp' app conn
