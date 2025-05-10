{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      :  Web.TwitchAPI.Helix.Client
Copyright   :  (c) Christina Wuest 2021-2025
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable
-}

module Web.TwitchAPI.Helix.Client
    ( Client
    , new, withManager, authenticate
    , doRequest, doRequest'
    ) where

import Prelude

import Data.Functor ( (<&>) )
import Data.Text    ( Text )
import Data.Aeson   ( FromJSON )

import qualified Data.Aeson              as JSON
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text ( encodeUtf8 )
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as TLS

import qualified Web.TwitchAPI.Helix.Request  as Req

data Client = Authenticated { bearerToken :: Text
                            , clientID :: Text
                            , manager :: HTTP.Manager
                            }
            | Unauthenticated { manager :: HTTP.Manager }

withManager :: HTTP.ManagerSettings -> IO Client
withManager settings = HTTP.newManager settings <&> Unauthenticated

new :: IO Client
new = withManager TLS.tlsManagerSettings

authenticate :: Client -> Text -> Text -> Client
authenticate (Unauthenticated{manager}) bearerToken clientID = Authenticated{..}
authenticate (Authenticated{manager}) bearerToken clientID = Authenticated{..}

doRequest :: Req.HelixRequest a => FromJSON b => Client -> a -> IO (Either String b)
doRequest c a = doRequest' c a <&> JSON.eitherDecode . HTTP.responseBody

doRequest' :: Req.HelixRequest a => Client -> a -> IO (HTTP.Response BS.ByteString)
doRequest' (Unauthenticated{manager=manager}) req =
    let request = Req.toRequest req
        fullReq = request { HTTP.requestHeaders = [ ("Content-Type", "application/json") ] }
    in HTTP.httpLbs fullReq manager
doRequest' (Authenticated{bearerToken=bearerToken, clientID=clientID, manager=manager}) req =
    let request = Req.toRequest req
        fullReq = request { HTTP.requestHeaders = [ ("Content-Type", "application/json")
                                                  , ("Authorization", Text.encodeUtf8 $ Text.append "Bearer " bearerToken)
                                                  , ("Client-Id", Text.encodeUtf8 clientID)
                                                  ] }
    in HTTP.httpLbs fullReq manager
