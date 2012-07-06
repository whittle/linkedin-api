{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.CompanyLookup
       ( CompanyLookupQuery(..)
       , CompanyFieldSelector(..)
       , CompanyLookupResult(..)
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response
import Network.API.LinkedIn --temp
import Network.API.ShimToken --temp

import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data CompanyLookupQuery = CompanyLookupQuery
                          { queryCompanyId :: Integer
                          , fieldSelectors :: [CompanyFieldSelector]
                          } deriving (Show)

instance Default CompanyLookupQuery where
  def = CompanyLookupQuery 0 allCompanyFieldSelectors

instance Query CompanyLookupQuery where
  toPathSegments q = [ "companies"
                     , (show $ queryCompanyId q)
                     -- ++ ":" ++ (outputFields $ fieldSelectors q)
                     ]
  toQueryItems = const []

data CompanyFieldSelector = Id | Name | UniversalName | EmailDomains
                          | CompanyType | Ticker | WebsiteUrl | Industry
                          | Status | LogoUrl | SquareLogoUrl | BlogRssUrl
                          | TwitterId | EmployeeCountRange | Specialties
                          | Locations [LocationFieldSelector]
                          | Description | StockExchange | FoundedYear
                          | EndYear | NumFollowers
                          deriving (Show)

allCompanyFieldSelectors = [ Id, Name, UniversalName
                             -- , EmailDomains
                             -- , CompanyType
                             -- , Ticker
                             -- , WebsiteUrl
                             -- , Industry
                             -- , Status
                             -- , LogoUrl
                             -- , SquareLogoUrl
                             -- , BlogRssUrl
                             -- , TwitterId
                             -- , EmployeeCountRange
                             -- , Specialties
                             -- , Description
                             -- , StockExchange
                             -- , FoundedYear
                             -- , EndYear
                             -- , NumFollowers
                             ]

outputField :: CompanyFieldSelector -> String
outputField Id = "id"
outputField Name = "name"
outputField UniversalName = "universal-name"
outputField EmailDomains = "email-domains"
outputField CompanyType = "company-type"
outputField Ticker = "ticker"
outputField WebsiteUrl = "website-url"
outputField Industry = "industry"
outputField Status = "status"
outputField LogoUrl = "logo-url"
outputField SquareLogoUrl = "square-logo-url"
outputField BlogRssUrl = "blog-rss-url"
outputField TwitterId = "twitter-id"
outputField EmployeeCountRange = "employee-count-range"
outputField Specialties = "specialties"
outputField (Locations _) = undefined
outputField Description = "description"
outputField StockExchange = "stock-exchange"
outputField FoundedYear = "founded-year"
outputField EndYear = "end-year"
outputField NumFollowers = "num-followers"

outputFields :: [CompanyFieldSelector] -> String
outputFields cs = "(" ++ fields' cs ++ ")"
  where fields' = intercalate "," . map outputField

type LocationFieldSelector = String

data CompanyLookupResult = CompanyLookupResult
                           { companyId :: Maybe Integer
                           , companyName :: Maybe Text
--                           , universalName :: Maybe Text
                           } deriving (Show)
parseCompanyLookupResult :: MonadThrow m => Sink Event m (Maybe CompanyLookupResult)
parseCompanyLookupResult = tagNoAttr "company" $ do
  id <- tagNoAttr "id" content
  name <- tagNoAttr "name" content
  return $ CompanyLookupResult (fmap (read . unpack) id) name

instance Response CompanyLookupResult where
  parsePage = parseCompanyLookupResult

instance QueryResponsePair CompanyLookupQuery CompanyLookupResult
