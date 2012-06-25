module Text.XML.Stream.Parse.Skip
       ( skipTag
       , skipContents
       ) where

import Data.Conduit
import Data.Text (Text)
import Text.XML.Stream.Parse
import Data.XML.Types

skipTag :: (MonadThrow m) => Text -> Sink Event m (Maybe ())
skipTag n = tagPredicate ((== n) . nameLocalName) ignoreAttrs $ const $ many (skipContents n) >> return ()

skipContents t = do
  x <- await
  case x of
    Just (EventEndElement n) | nameLocalName n == t -> Done x Nothing
    Nothing -> Done x Nothing
    _ -> return (Just ())
