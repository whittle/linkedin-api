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

data CompanyFieldSelector = IdS | NameS | UniversalNameS | EmailDomainsS
                          | CompanyTypeS | TickerS | WebsiteUrlS | IndustryS
                          | StatusS | LogoUrlS | SquareLogoUrlS | BlogRssUrlS
                          | TwitterIdS | EmployeeCountRangeS | SpecialtiesS
                          | LocationsS [LocationFieldSelector] | DescriptionS
                          | StockExchangeS | FoundedYearS | EndYearS
                          | NumFollowersS
                          deriving (Show)

allCompanyFieldSelectors = [ IdS, NameS, UniversalNameS, EmailDomainsS
                           , CompanyTypeS, TickerS, WebsiteUrlS, IndustryS
                           , StatusS, LogoUrlS, SquareLogoUrlS, BlogRssUrlS
                           , TwitterIdS, EmployeeCountRangeS, SpecialtiesS
                           , DescriptionS, StockExchangeS, FoundedYearS
                           , EndYearS, NumFollowersS
                           ]

outputField :: CompanyFieldSelector -> String
outputField IdS = "id"
outputField NameS = "name"
outputField UniversalNameS = "universal-name"
outputField EmailDomainsS = "email-domains"
outputField CompanyTypeS = "company-type"
outputField TickerS = "ticker"
outputField WebsiteUrlS = "website-url"
outputField IndustryS = "industry"
outputField StatusS = "status"
outputField LogoUrlS = "logo-url"
outputField SquareLogoUrlS = "square-logo-url"
outputField BlogRssUrlS = "blog-rss-url"
outputField TwitterIdS = "twitter-id"
outputField EmployeeCountRangeS = "employee-count-range"
outputField SpecialtiesS = "specialties"
outputField (LocationsS _) = undefined
outputField DescriptionS = "description"
outputField StockExchangeS = "stock-exchange"
outputField FoundedYearS = "founded-year"
outputField EndYearS = "end-year"
outputField NumFollowersS = "num-followers"

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
                           , twitterId :: Maybe Text
                           , employeeCountRange :: Maybe EmployeeCountRange
                           , specialties :: Maybe Specialties
                           , description :: Maybe Text
                           , stockExchange :: Maybe StockExchange
                           , foundedYear :: Maybe Integer
                           , endYear :: Maybe Integer
                           , numFollowers :: Maybe Integer
                           } deriving (Show)
parseCompanyLookupResult :: MonadThrow m => Sink Event m (Maybe CompanyLookupResult)
parseCompanyLookupResult = tagNoAttr "company" $ CompanyLookupResult
                           <$> (fmap (fmap (read . unpack)) $ selNoAttr IdS content)
                           <*> (fmap join $ selNoAttr NameS contentMaybe)
                           <*> (fmap join $ selNoAttr UniversalNameS contentMaybe)
                           <*> parseEmailDomains
                           <*> parseCompanyType
                           <*> (fmap join $ selNoAttr TickerS contentMaybe)
                           <*> (fmap join $ selNoAttr WebsiteUrlS contentMaybe)
                           <*> (fmap join $ selNoAttr IndustryS contentMaybe)
                           <*> parseCompanyStatus
                           <*> (fmap join $ selNoAttr LogoUrlS contentMaybe)
                           <*> (fmap join $ selNoAttr SquareLogoUrlS contentMaybe)
                           <*> (fmap join $ selNoAttr BlogRssUrlS contentMaybe)
                           <*> (fmap join $ selNoAttr TwitterIdS contentMaybe)
                           <*> parseEmployeeCountRange
                           <*> parseSpecialties
                           <*> (fmap join $ selNoAttr DescriptionS contentMaybe)
                           <*> parseStockExchange
                           <*> (fmap (fmap (read . unpack)) $ selNoAttr FoundedYearS content)
                           <*> (fmap (fmap (read . unpack)) $ selNoAttr EndYearS content)
                           <*> (fmap (fmap (read . unpack)) $ selNoAttr NumFollowersS content)

instance Response CompanyLookupResult where
  parsePage = parseCompanyLookupResult

instance QueryResponsePair CompanyLookupQuery CompanyLookupResult

data EmailDomains = EmailDomains
                    { totalEmailDomains :: Integer
                    , allEmailsDomains :: [Text]
                    } deriving (Show)
parseEmailDomains :: MonadThrow m => Sink Event m (Maybe EmailDomains)
parseEmailDomains = selName EmailDomainsS (requireAttr "total") $ \t -> do
  domains <- many $ tagNoAttr "email-domain" content
  return $ EmailDomains (read $ unpack t) domains

data CompanyType = CompanyType
                   { companyTypeCode :: Text
                   , companyTypeName :: Text
                   } deriving (Show)
parseCompanyType :: MonadThrow m => Sink Event m (Maybe CompanyType)
parseCompanyType = selNoAttr CompanyTypeS $ CompanyType
                   <$> (force "company-type must contain a code" $ tagNoAttr "code" content)
                   <*> (force "company-type must contain a name" $ tagNoAttr "name" content)

data CompanyStatus = CompanyStatus
                     { companyStatusCode :: Text
                     , companyStatusName :: Text
                     } deriving (Show)
parseCompanyStatus :: MonadThrow m => Sink Event m (Maybe CompanyStatus)
parseCompanyStatus = selNoAttr StatusS $ CompanyStatus
                     <$> (force "status must contain a code" $ tagNoAttr "code" content)
                     <*> (force "status must contain a name" $ tagNoAttr "name" content)

data EmployeeCountRange = EmployeeCountRange
                          { employeeCountRangeCode :: Text
                          , employeeCountRangeName :: Text
                          } deriving (Show)
parseEmployeeCountRange :: MonadThrow m => Sink Event m (Maybe EmployeeCountRange)
parseEmployeeCountRange = selNoAttr EmployeeCountRangeS $ EmployeeCountRange
                          <$> (force (name++" must contain a code") $ tagNoAttr "code" content)
                          <*> (force (name++" must contain a name") $ tagNoAttr "name" content)
  where name = outputField EmployeeCountRangeS

data Specialties = Specialties
                   { totalSpecialties :: Integer
                   , allSpecialties :: [Text]
                   } deriving (Show)
parseSpecialties :: MonadThrow m => Sink Event m (Maybe Specialties)
parseSpecialties = selName SpecialtiesS (requireAttr "total") $ \t -> do
  specialties <- many $ tagNoAttr "specialty" content
  return $ Specialties (read $ unpack t) specialties

data StockExchange = StockExchange
                     { stockExchangeCode :: Text
                     , stockExchangeName :: Text
                     } deriving (Show)
parseStockExchange :: MonadThrow m => Sink Event m (Maybe StockExchange)
parseStockExchange = selNoAttr StockExchangeS $ StockExchange
                     <$> (force (name ++ " must contain a code") $ tagNoAttr "code" content)
                     <*> (force (name ++ " must contain a name") $ tagNoAttr "name" content)
  where name = outputField StockExchangeS
