module Network.API.ShimToken
       ( shimToken
       ) where

import Network.OAuth.Consumer (Token)
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as B

shimToken :: IO Token
shimToken = fmap decode $ B.readFile "./shim_token"
