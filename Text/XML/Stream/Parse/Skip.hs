{-# LANGUAGE PatternGuards #-}
module Text.XML.Stream.Parse.Skip
       ( skipTag
       , skipContents
       , readM
       ) where

import Data.Conduit (MonadThrow, Sink, Pipe, await)
import Data.Text (Text, unpack)
import Text.XML.Stream.Parse
import Data.XML.Types

skipTag :: MonadThrow m => Text -> Sink Event m (Maybe ())
skipTag n = tagPredicate ((== n) . nameLocalName) ignoreAttrs $ const $ many (skipContents n) >> return ()

skipContents :: Monad m => Text -> Pipe l Event o u m (Maybe ())
skipContents t = do
  x <- await
  case x of
    Just (EventEndElement n) | nameLocalName n == t -> return Nothing
    Nothing -> return Nothing
    _ -> return (Just ())

readM :: (Monad m) => Text -> m Integer
readM t | [x] <- parse = return x
        | otherwise = fail $ "Failed to parse \"" ++ s ++ "\" as an Integer."
  where s = unpack t
        parse = [x | (x, _) <- reads s]
