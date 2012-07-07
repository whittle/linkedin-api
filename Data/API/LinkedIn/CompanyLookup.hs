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
import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.XML.Types (Event, Name(..))
import Text.XML.Stream.Parse

data CompanyLookupQuery = CompanyLookupQuery
                          { queryCompanyId :: Integer
                          , fieldSelectors :: [CompanyFieldSelector]
                          } deriving (Show)

instance Default CompanyLookupQuery where
  def = CompanyLookupQuery 0 allCompanyFieldSelectors

instance Query CompanyLookupQuery where
  toPathSegments q = [ "companies"
                     , (show $ queryCompanyId q) ++ ":" ++ (outputFields $ fieldSelectors q)
                     ]
  toQueryItems = const []

data CompanyFieldSelector = IdSelector | NameSelector | UniversalNameSelector
                          | EmailDomainsSelector | CompanyTypeSelector
                          | TickerSelector | WebsiteUrlSelector
                          | IndustrySelector | StatusSelector | LogoUrlSelector
                          | SquareLogoUrlSelector | BlogRssUrlSelector
                          | TwitterIdSelector | EmployeeCountRangeSelector
                          | SpecialtiesSelector
                          | LocationsSelector [LocationFieldSelector]
                          | DescriptionSelector | StockExchangeSelector
                          | FoundedYearSelector | EndYearSelector
                          | NumFollowersSelector
                          deriving (Show)

allCompanyFieldSelectors = [ IdSelector, NameSelector, UniversalNameSelector
                           , EmailDomainsSelector, CompanyTypeSelector
                           , TickerSelector, WebsiteUrlSelector
                           , IndustrySelector, StatusSelector, LogoUrlSelector
                           , SquareLogoUrlSelector, BlogRssUrlSelector
                             -- , TwitterIdSelector
                             -- , EmployeeCountRangeSelector
                             -- , SpecialtiesSelector
                             -- , DescriptionSelector
                             -- , StockExchangeSelector
                             -- , FoundedYearSelector
                             -- , EndYearSelector
                             -- , NumFollowersSelector
                             ]

outputField :: CompanyFieldSelector -> String
outputField IdSelector = "id"
outputField NameSelector = "name"
outputField UniversalNameSelector = "universal-name"
outputField EmailDomainsSelector = "email-domains"
outputField CompanyTypeSelector = "company-type"
outputField TickerSelector = "ticker"
outputField WebsiteUrlSelector = "website-url"
outputField IndustrySelector = "industry"
outputField StatusSelector = "status"
outputField LogoUrlSelector = "logo-url"
outputField SquareLogoUrlSelector = "square-logo-url"
outputField BlogRssUrlSelector = "blog-rss-url"
outputField TwitterIdSelector = "twitter-id"
outputField EmployeeCountRangeSelector = "employee-count-range"
outputField SpecialtiesSelector = "specialties"
outputField (LocationsSelector _) = undefined
outputField DescriptionSelector = "description"
outputField StockExchangeSelector = "stock-exchange"
outputField FoundedYearSelector = "founded-year"
outputField EndYearSelector = "end-year"
outputField NumFollowersSelector = "num-followers"

toName :: CompanyFieldSelector -> Name
toName s = Name (pack $ outputField s) Nothing Nothing

selNoAttr :: MonadThrow m => CompanyFieldSelector
             -> Sink Event m a -> Sink Event m (Maybe a)
selNoAttr = tagNoAttr . toName

selName :: MonadThrow m => CompanyFieldSelector -> AttrParser a
           -> (a -> Sink Event m b) -> Sink Event m (Maybe b)
selName = tagName . toName

outputFields :: [CompanyFieldSelector] -> String
outputFields cs = "(" ++ fields' cs ++ ")"
  where fields' = intercalate "," . map outputField

type LocationFieldSelector = String

data CompanyLookupResult = CompanyLookupResult
                           { companyId :: Maybe Integer
                           , companyName :: Maybe Text
                           , universalName :: Maybe Text
                           , emailDomains :: Maybe EmailDomains
                           , companyType :: Maybe CompanyType
                           , tickerSymbol :: Maybe Text
                           , websiteUrl :: Maybe Text
                           , companyIndustry :: Maybe Text
                           , companyStatus :: Maybe CompanyStatus
                           , logoUrl :: Maybe Text
                           , squareLogoUrl :: Maybe Text
                           , blogRssUrl :: Maybe Text
                           } deriving (Show)
parseCompanyLookupResult :: MonadThrow m => Sink Event m (Maybe CompanyLookupResult)
parseCompanyLookupResult = tagNoAttr "company" $ CompanyLookupResult
                           <$> (fmap (fmap (read . unpack)) $ selNoAttr IdSelector content)
                           <*> (fmap join $ selNoAttr NameSelector contentMaybe)
                           <*> (fmap join $ selNoAttr UniversalNameSelector contentMaybe)
                           <*> parseEmailDomains
                           <*> parseCompanyType
                           <*> (fmap join $ selNoAttr TickerSelector contentMaybe)
                           <*> (fmap join $ selNoAttr WebsiteUrlSelector contentMaybe)
                           <*> (fmap join $ selNoAttr IndustrySelector contentMaybe)
                           <*> parseCompanyStatus
                           <*> (fmap join $ selNoAttr LogoUrlSelector contentMaybe)
                           <*> (fmap join $ selNoAttr SquareLogoUrlSelector contentMaybe)
                           <*> (fmap join $ selNoAttr BlogRssUrlSelector contentMaybe)

instance Response CompanyLookupResult where
  parsePage = parseCompanyLookupResult

instance QueryResponsePair CompanyLookupQuery CompanyLookupResult

data EmailDomains = EmailDomains
                    { totalEmailDomains :: Integer
                    , allEmailsDomains :: [Text]
                    } deriving (Show)
parseEmailDomains :: MonadThrow m => Sink Event m (Maybe EmailDomains)
parseEmailDomains = selName EmailDomainsSelector (requireAttr "total") $ \t -> do
  domains <- many $ tagNoAttr "email-domain" content
  return $ EmailDomains (read $ unpack t) domains

data CompanyType = CompanyType
                   { companyTypeCode :: Text
                   , companyTypeName :: Text
                   } deriving (Show)
parseCompanyType :: MonadThrow m => Sink Event m (Maybe CompanyType)
parseCompanyType = selNoAttr CompanyTypeSelector $ CompanyType
                   <$> (force "company-type must contain a code" $ tagNoAttr "code" content)
                   <*> (force "company-type must contain a name" $ tagNoAttr "name" content)

data CompanyStatus = CompanyStatus
                     { companyStatusCode :: Text
                     , companyStatusName :: Text
                     } deriving (Show)
parseCompanyStatus :: MonadThrow m => Sink Event m (Maybe CompanyStatus)
parseCompanyStatus = selNoAttr StatusSelector $ CompanyStatus
                     <$> (force "status must contain a code" $ tagNoAttr "code" content)
                     <*> (force "status must contain a name" $ tagNoAttr "name" content)
