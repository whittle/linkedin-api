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
import Network.API.LinkedIn --temp
import Network.API.ShimToken --temp
import Text.XML.Stream.Parse.Skip (skipTag, skipContents, readM) --temp

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
                          , facet :: Maybe Facet
                          , facets :: [Facet]
                          , start :: Maybe Integer
                          , count :: Maybe Integer
                          , sort :: Maybe SortOrder
                          } deriving (Eq, Show)

instance Default CompanySearchQuery where
  def = CompanySearchQuery { keywords = []
                           , hqOnly = Nothing
                           , facet = Nothing
                           , facets = []
                           , start = Nothing
                           , count = Nothing
                           , sort = Nothing
                           }

instance Query CompanySearchQuery where
  toPathSegments _ = ["company-search"]
  toQueryItems q = [("keywords", intercalate " " $ map unpack $ keywords q)]

type Facet = Text

data SortOrder = Relevance
               | Relationship
               | Followers
               | CompanySize
               deriving (Eq, Show)


data CompanySearchPage = CompanySearchPage
                         { searchCompanies :: Companies
                         , numResults :: Integer
                         -- , searchFacets :: Maybe Facets
                         } deriving (Show)
parseCompanySearchPage :: MonadThrow m => Sink Event m (Maybe CompanySearchPage)
parseCompanySearchPage = tagNoAttr "company-search" $ do
  companyList <- force "companies required" parseCompanies
  numResults <- fmap (read . unpack) $ force "numResults required"
                $ tagNoAttr "num-results" content
  skipTag "facets"
  return $ CompanySearchPage companyList numResults

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
            deriving (Show)
parseFacets :: MonadThrow m => Sink Event m (Maybe Facets)
parseFacets = tagName "facets" ignoreAttrs $ const $ many (skipContents "facets") >> return Facets
