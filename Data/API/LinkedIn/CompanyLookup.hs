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
                             -- , TickerSelector
                             -- , WebsiteUrlSelector
                             -- , IndustrySelector
                             -- , StatusSelector
                             -- , LogoUrlSelector
                             -- , SquareLogoUrlSelector
                             -- , BlogRssUrlSelector
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
                           } deriving (Show)
parseCompanyLookupResult :: MonadThrow m => Sink Event m (Maybe CompanyLookupResult)
parseCompanyLookupResult = tagNoAttr "company" $ do
  mCompanyId <- tagNoAttr (toName IdSelector) content
  mCompanyName <- tagNoAttr (toName NameSelector) content
  mUniversalName <- tagNoAttr (toName UniversalNameSelector) content
  mEmailDomains <- parseEmailDomains
  mCompanyType <- parseCompanyType
  return $ CompanyLookupResult (fmap (read . unpack) mCompanyId) mCompanyName mUniversalName mEmailDomains mCompanyType

instance Response CompanyLookupResult where
  parsePage = parseCompanyLookupResult

instance QueryResponsePair CompanyLookupQuery CompanyLookupResult

data EmailDomains = EmailDomains
                    { totalEmailDomains :: Integer
                    , allEmailsDomains :: [EmailDomain]
                    } deriving (Show)
parseEmailDomains :: MonadThrow m => Sink Event m (Maybe EmailDomains)
parseEmailDomains = tagName (toName EmailDomainsSelector) (requireAttr "total") $ \t -> do
  domains <- many parseEmailDomain
  return $ EmailDomains (read $ unpack t) domains

newtype EmailDomain = EmailDomain
                      { emailDomainText :: Text
                      } deriving (Show)
parseEmailDomain :: MonadThrow m => Sink Event m (Maybe EmailDomain)
parseEmailDomain = tagNoAttr "email-domain" content >>= return . fmap EmailDomain

data CompanyType = CompanyType
                   { companyTypeCode :: Text
                   , companyTypeName :: Text
                   } deriving (Show)
parseCompanyType :: MonadThrow m => Sink Event m (Maybe CompanyType)
parseCompanyType = tagNoAttr (toName CompanyTypeSelector) $ do
  code <- force "company-type nodes must have a code node" $ tagNoAttr "code" content
  name <- force "company-type nodes must have a name node" $ tagNoAttr "name" content
  return $ CompanyType code name
