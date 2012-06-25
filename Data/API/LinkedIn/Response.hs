module Data.API.LinkedIn.Response
       ( Response(..)
       ) where

import Data.Conduit (MonadThrow, Sink)
import Data.XML.Types (Event)

class Response r where
  parsePage :: MonadThrow m => Sink Event m (Maybe r)
