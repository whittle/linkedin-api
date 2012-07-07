{-# LANGUAGE OverloadedStrings #-}
module Data.API.LinkedIn.Facet
       ( QueryFacet
       , Facets(..)
       , parseFacets
       , Facet(..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (MonadThrow, Sink)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

type QueryFacet = Text --temp

data Facets = Facets
              { totalFacets :: Integer
              , allFacets :: [Facet]
              } deriving (Show)
parseFacets :: MonadThrow m => Sink Event m (Maybe Facets)
parseFacets = tagName "facets" (requireAttr "total") $ \total -> do
  facets <- many $ parseFacet
  return $ Facets (read $ unpack total) facets

data Facet = Facet
             { facetCode :: Text
             , facetName :: Text
             , facetBuckets :: Maybe Buckets
             } deriving (Show)
parseFacet :: MonadThrow m => Sink Event m (Maybe Facet)
parseFacet = tagNoAttr "facet" $ Facet
             <$> (force "facet must contain a code" $ tagNoAttr "code" content)
             <*> (force "facet must contain a name" $ tagNoAttr "name" content)
             <*> parseBuckets

data Buckets = Buckets
               { totalBuckets :: Integer
               , allBuckets :: [Bucket]
               } deriving (Show)
parseBuckets :: MonadThrow m => Sink Event m (Maybe Buckets)
parseBuckets = tagName "buckets" (requireAttr "total") $ \total -> do
  buckets <- many $ parseBucket
  return $ Buckets (read $ unpack total) buckets

data Bucket = Bucket
              { bucketCode :: Text
              , bucketName :: Text
              , bucketCount :: Integer
              , selected :: Bool
              } deriving (Show)
parseBucket :: MonadThrow m => Sink Event m (Maybe Bucket)
parseBucket = tagNoAttr "bucket" $ Bucket
              <$> (force "bucket must contain code" $ tagNoAttr "code" content)
              <*> (force "bucket must contain name" $ tagNoAttr "name" content)
              <*> (fmap (read . unpack) $ force "bucket must contain a count"
                   $ tagNoAttr "count" content)
              <*> (fmap ("true"==) $ force "bucket must contain a selected"
                   $ tagNoAttr "selected" content)
