{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.PeopleSearch
       ( PeopleSearchQuery(..)
       , PeopleSearchPage(..)
       , People(..)
       , Person(..)
       ) where

import Data.API.LinkedIn.Facet
import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response
import Network.API.LinkedIn --temp
import Network.API.ShimToken --temp

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data PeopleSearchQuery = PeopleSearchQuery
                         { keywords :: Text
                         , queryFirstName :: Maybe Text
                         , queryLastName :: Maybe Text
                         , queryCompanyName :: Maybe Text
                         , queryCurrentCompany :: Maybe Bool
                         , queryTitle :: Maybe Text
                         , queryCurrentTitle :: Maybe Bool
                         , querySchoolName :: Maybe Text
                         , queryCurrentSchool :: Maybe Bool
                         , queryCountryCode :: Maybe Text
                         , queryPostalCode :: Maybe Text
                         , queryDistance :: Maybe Integer
                         , queryFacet :: Maybe QueryFacet
                         , queryFacets :: [QueryFacet]
                         , queryStart :: Maybe Integer
                         , queryCount :: Maybe Integer
                         , querySort :: Maybe Text
                         } deriving (Eq, Show)

instance Default PeopleSearchQuery where
  def = PeopleSearchQuery "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing

instance Query PeopleSearchQuery where
  toPathSegments = const ["people-search"]
  toQueryItems q = [("keywords", unpack $ keywords q)]

data PeopleSearchPage = PeopleSearchPage
                        { people :: People
                        , numResults :: Maybe Integer
                        , facets :: Maybe Facets
                        } deriving (Show)

instance Response PeopleSearchPage where
  parsePage = tagNoAttr "people-search" $ PeopleSearchPage
              <$> force "people-search must include a people" parsePeople
              <*> (fmap (fmap (read . unpack)) $ tagNoAttr "num-results" content)
              <*> parseFacets

instance QueryResponsePair PeopleSearchQuery PeopleSearchPage

data People = People
              { peopleTotal :: Integer
              , peopleCount :: Maybe Integer
              , peopleStart :: Maybe Integer
              , allPeople :: [Person]
              } deriving (Show)
parsePeople :: MonadThrow m => Sink Event m (Maybe People)
parsePeople = tagName "people" tcsAttr $ \(t, c, s) -> do
  people <- many $ parsePerson
  return $ People t c s people
  where tcsAttr = (,,) <$> reqIntAttr "total" <*> optIntAttr "count" <*> optIntAttr "start"
        reqIntAttr = fmap (read . unpack) . requireAttr
        optIntAttr = fmap (fmap (read . unpack)) . optionalAttr

data Person = Person
              { personId :: Text
              , firstName :: Text
              , lastName :: Text
              } deriving (Show)
parsePerson :: MonadThrow m => Sink Event m (Maybe Person)
parsePerson = tagNoAttr "person" $ Person
              <$> (force "person must include an id" $ tagNoAttr "id" content)
              <*> (force "person must include a first-name" $ tagNoAttr "first-name" content)
              <*> (force "person must include a last-name" $ tagNoAttr "last-name" content)
