{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.CompanySearch
       ( CompanySearchQuery(..)
       , CompanySearchPage(..)
       , Companies(..)
       , Company(..)
       ) where

import Data.API.LinkedIn.Facet
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
                          { queryKeywords :: Text
                          , queryHqOnly :: Maybe Bool
                          , queryFacet :: Maybe QueryFacet
                          , queryFacets :: [QueryFacet]
                          , queryStart :: Maybe Integer
                          , queryCount :: Maybe Integer
                          , querySort :: Maybe SortOrder
                          } deriving (Eq, Show)

instance Default CompanySearchQuery where
  def = CompanySearchQuery { queryKeywords = ""
                           , queryHqOnly = Nothing
                           , queryFacet = Nothing
                           , queryFacets = []
                           , queryStart = Nothing
                           , queryCount = Nothing
                           , querySort = Nothing
                           }

instance Query CompanySearchQuery where
  toPathSegments _ = ["company-search"]
  toQueryItems q = [("keywords", unpack $ queryKeywords q)]

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
