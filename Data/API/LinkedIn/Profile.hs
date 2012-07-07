{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings #-}
module Data.API.LinkedIn.Profile
       ( ProfileQuery
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data ProfileQuery = ProfileQuery
                    { queryMemberId :: Text
                    } deriving (Show)

instance Default ProfileQuery where
  def = ProfileQuery ""

instance Query ProfileQuery where
  toPathSegments q = ["people", "id=" ++ (unpack $ queryMemberId q)]
  toQueryItems = const []

data ProfileResult = ProfileResult
                     { firstName :: Text
                     , lastName :: Text
                     , headline :: Text
                     , siteStandardProfileRequest :: SiteStandardProfileRequest
                     } deriving (Show)

instance Response ProfileResult where
  parsePage = tagNoAttr "person" $ ProfileResult
              <$> (force "person must have a first-name" $ tagNoAttr "first-name" content)
              <*> (force "person must have a last-name" $ tagNoAttr "last-name" content)
              <*> (force "person must have a headline" $ tagNoAttr "headline" content)
              <*> force "person must have a site-standard-profile-request" parseSiteStandardProfileRequest

instance QueryResponsePair ProfileQuery ProfileResult

data SiteStandardProfileRequest = SiteStandardProfileRequest
                                  { siteStandardProfileRequestUrl :: Text
                                  } deriving (Show)
parseSiteStandardProfileRequest :: MonadThrow m => Sink Event m (Maybe SiteStandardProfileRequest)
parseSiteStandardProfileRequest = tagNoAttr "site-standard-profile-request" $ SiteStandardProfileRequest
                                  <$> (force "site-standard-profile-request must contain an url" $ tagNoAttr "url" content)
