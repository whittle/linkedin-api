module Network.API.LinkedIn
       ( sendRequest
       , mkRequest
       , module Data.API.LinkedIn.CompanySearch
       ) where

import Data.API.LinkedIn.Query
import qualified Data.API.LinkedIn.Response as L
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.CompanySearch
import Network.API.ShimToken

import Data.Maybe (fromJust)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import qualified Network.OAuth.Http.Response as H
import Network.OAuth.Http.CurlHttpClient

import Control.Monad.Trans.Resource
import Data.Conduit (($$))
import Text.XML.Stream.Parse

getResponse :: ( QueryResponsePair q r
               , Query q
               , L.Response r
               ) => Token
               -> q
               -> IO r
getResponse token query = (sendRequest token $ mkRequest query) >>= parseResponse

parseResponse :: (L.Response r) => H.Response -> IO r
parseResponse r = runResourceT $ parseLBS def (H.rspPayload r) $$ force "something required" L.parsePage

-- |Signs a request with the OAuth Token and performs it using Curl.
sendRequest :: Token -> Request -> IO H.Response
sendRequest token request = runOAuthM token $ (signRq2 HMACSHA1 Nothing request >>= serviceRequest CurlClient)

-- |Creates an HTTP GET request from any Query.
mkRequest :: (Query q) => q -> Request
mkRequest query = baseURL { reqHeaders = baseHeaders
                          , pathComps = toPathComps query
                          , qString = toQueryStr query
                          }

baseURL = fromJust $ parseURL $ "http://api.linkedin.com/"

baseHeaders = fromList [("x-li-format", "xml")]
