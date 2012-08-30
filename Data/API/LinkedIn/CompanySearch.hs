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
                          , companyFieldSelectors :: [CompanyFieldSelector]
                          , queryHqOnly :: Maybe Bool
                          , queryFacet :: Maybe QueryFacet
                          , queryFacets :: [QueryFacet]
                          , queryStart :: Maybe Integer
                          , queryCount :: Maybe Integer
                          , querySort :: Maybe SortOrder
                          } deriving (Eq, Show)

instance Default CompanySearchQuery where
  def = CompanySearchQuery "" [IdSelector, NameSelector, UniversalNameSelector, WebsiteUrlSelector, LogoUrlSelector] Nothing Nothing [] Nothing Nothing Nothing

instance Query CompanySearchQuery where
  toPathSegments = (:[]) . ("company-search"++) . toSelectorString . companyFieldSelectors
  toQueryItems q = [("keywords", unpack $ queryKeywords q)]

data CompanyFieldSelector = IdSelector
                          | NameSelector
                          | UniversalNameSelector
                          | WebsiteUrlSelector
                          | LogoUrlSelector
                          deriving (Eq, Show)

toSelectorPiece :: CompanyFieldSelector -> String
toSelectorPiece IdSelector = "id"
toSelectorPiece NameSelector = "name"
toSelectorPiece UniversalNameSelector = "universal-name"
toSelectorPiece WebsiteUrlSelector = "website-url"
toSelectorPiece LogoUrlSelector = "logo-url"

toSelectorString :: [CompanyFieldSelector] -> String
toSelectorString = (":(companies:("++) . (++"),num-results)") . intercalate "," . map toSelectorPiece

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
                       , companyUniversalName :: Maybe Text
                       , companyLogoUrl :: Maybe Text
                       , companyWebsiteUrl :: Maybe Text
                       } deriving (Show)
parseCompany :: MonadThrow m => Sink Event m (Maybe Company)
parseCompany = tagNoAttr "company" $ Company
               <$> (fmap (read . unpack) $ force "companyId required"
                    $ tagNoAttr "id" content)
               <*> (force "company requires name" $ tagNoAttr "name" content)
               <*> tagNoAttr "universal-name" content
               <*> tagNoAttr "logo-url" content
               <*> tagNoAttr "website-url" content
