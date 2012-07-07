{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.CompanySearch
       ( CompanySearchQuery(..)
       , CompanySearchPage(..)
       , Companies(..)
       , Company(..)
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data CompanySearchQuery = CompanySearchQuery
                          { keywords :: [Text]
                          , hqOnly :: Maybe Bool
                          , queryFacet :: Maybe QueryFacet
                          , queryFacets :: [QueryFacet]
                          , start :: Maybe Integer
                          , count :: Maybe Integer
                          , sort :: Maybe SortOrder
                          } deriving (Eq, Show)

instance Default CompanySearchQuery where
  def = CompanySearchQuery { keywords = []
                           , hqOnly = Nothing
                           , queryFacet = Nothing
                           , queryFacets = []
                           , start = Nothing
                           , count = Nothing
                           , sort = Nothing
                           }

instance Query CompanySearchQuery where
  toPathSegments _ = ["company-search"]
  toQueryItems q = [("keywords", intercalate " " $ map unpack $ keywords q)]

type QueryFacet = Text

data SortOrder = Relevance
               | Relationship
               | Followers
               | CompanySize
               deriving (Eq, Show)

data CompanySearchPage = CompanySearchPage
                         { companies :: Companies
                         , numResults :: Integer
                         , resultFacets :: Maybe Facets
                         } deriving (Show)
parseCompanySearchPage :: MonadThrow m => Sink Event m (Maybe CompanySearchPage)
parseCompanySearchPage = tagNoAttr "company-search" $ CompanySearchPage
                         <$> force "companies required" parseCompanies
                         <*> (fmap (read . unpack) $ force "numResults required"
                              $ tagNoAttr "num-results" content)
                         <*> parseFacets

instance Response CompanySearchPage where
  parsePage = parseCompanySearchPage

instance QueryResponsePair CompanySearchQuery CompanySearchPage

data Companies = Companies [Company]
                 deriving (Show)
parseCompanies :: MonadThrow m => Sink Event m (Maybe Companies)
parseCompanies = tagName "companies" (ignoreAttrs) $ \a -> fmap Companies
                                                           $ many parseCompany

data Company = Company { companyId :: Integer
                       , companyName :: Text
                       } deriving (Show)
parseCompany :: MonadThrow m => Sink Event m (Maybe Company)
parseCompany = tagNoAttr "company" $ Company
               <$> (fmap (read . unpack) $ force "companyId required"
                    $ tagNoAttr "id" content)
               <*> (force "companyName required" $ tagNoAttr "name" content)

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
