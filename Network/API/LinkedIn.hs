module Network.API.LinkedIn
       () where

import Data.Maybe (fromJust)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Network.API.ShimToken
import Data.API.LinkedIn.Query

-- |Signs a request with the OAuth Token and performs it using Curl.
sendRequest :: Token -> Request -> IO Response
sendRequest token request = runOAuthM token $ (signRq2 HMACSHA1 Nothing request >>= serviceRequest CurlClient)

-- |Creates an HTTP GET request from any Query.
mkRequest :: (Query q) => q -> Request
mkRequest query = baseURL { reqHeaders = baseHeaders
                          , pathComps = toPathComps query
                          , qString = toQueryStr query
                          }

baseURL = fromJust $ parseURL $ "http://api.linkedin.com/"

baseHeaders = fromList [("x-li-format", "xml")]
