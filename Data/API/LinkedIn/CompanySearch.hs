{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.CompanySearch
       ( CompanySearchQuery(..)
       , CompanySearchPage(..)
       , Company(..)
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response
import Text.XML.Stream.Parse.Skip (skipTag, skipContents, readM)

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


data CompanySearchPage = CompanySearchPage Companies --NumResults --(Maybe Facets)
                       deriving (Show)
parseCompanySearchPage :: MonadThrow m => Sink Event m (Maybe CompanySearchPage)
parseCompanySearchPage = tagNoAttr "company-search" $ do
  companyList <- force "companies required" parseCompanies
  skipTag "num-results"
  --numResults <- force "numResults required" parseNumResults
  skipTag "facets"
  --facets <- parseFacets
  return $ CompanySearchPage companyList --numResults --facets

instance Response CompanySearchPage where
  parsePage = parseCompanySearchPage

data Companies = Companies [Company]
                 deriving (Show)
parseCompanies :: MonadThrow m => Sink Event m (Maybe Companies)
parseCompanies = tagName "companies" (ignoreAttrs) $ \a -> fmap Companies $ many parseCompany

data Company = Company { companyId :: Integer
                       , companyName :: Text
                       } deriving (Show)
parseCompany :: MonadThrow m => Sink Event m (Maybe Company)
parseCompany = tagNoAttr "company" $ do
  id <- force "companyId required" $ tagNoAttr "id" content
  name <- force "companyName required" $ tagNoAttr "name" content
  return $ Company (read $ unpack id) name

data NumResults = NumResults Integer
                deriving (Show)
-- parseNumResults :: MonadThrow m => Sink Event m (Maybe NumResults)
-- parseNumResults = fmap (fmap NumResults) $ force "num results required" $ tagNoAttr "num-results" $ content >>= readM

data Facets = Facets
            deriving (Show)
parseFacets :: MonadThrow m => Sink Event m (Maybe Facets)
parseFacets = tagName "facets" ignoreAttrs $ const $ many (skipContents "facets") >> return Facets

instance QueryResponsePair CompanySearchQuery CompanySearchPage
