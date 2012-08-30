{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses #-}
module Data.API.LinkedIn.PeopleSearch
       ( PeopleSearchQuery(..)
       , PeopleSearchTerm(..)
       , PeopleSearchPage(..)
       , People(..)
       , Person(..)
       ) where

import Data.API.LinkedIn.Facet
import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response

import Control.Applicative ((<$>), (<*>))
import Data.Char (toLower)
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data PeopleSearchQuery = PeopleSearchQuery
                         { peopleSearchTerms :: [PeopleSearchTerm]
                         , peopleFieldSelectors :: [PeopleFieldSelector]
                         , queryFacet :: Maybe QueryFacet
                         , queryFacets :: [QueryFacet]
                         , queryStart :: Maybe Integer
                         , queryCount :: Maybe Integer
                         , querySort :: Maybe Text
                         } deriving (Eq, Show)

instance Default PeopleSearchQuery where
  def = PeopleSearchQuery [] [IdSelector, FirstNameSelector, LastNameSelector, HeadlineSelector, PictureUrlSelector] Nothing [] Nothing Nothing Nothing

instance Query PeopleSearchQuery where
  toPathSegments = (:[]) . ("people-search"++) . toSelectorString . peopleFieldSelectors
  toQueryItems = map toQueryItem . peopleSearchTerms

data PeopleSearchTerm = KeywordsTerm Text
                      | FirstNameTerm Text
                      | LastNameTerm Text
                      | CompanyNameTerm Text
                      | CurrentCompanyTerm Bool
                      | TitleTerm Text
                      | CurrentTitleTerm Bool
                      | SchoolNameTerm Text
                      | CurrentSchoolTerm Bool
                      | CountryCodeTerm Text
                      | PostalCodeTerm Text
                      | DistanceTerm Integer
                      deriving (Eq, Show)

toQueryItem :: PeopleSearchTerm -> (String, String)
toQueryItem (KeywordsTerm t) = ("keywords", unpack t)
toQueryItem (FirstNameTerm t) = ("first-name", unpack t)
toQueryItem (LastNameTerm t) = ("last-name", unpack t)
toQueryItem (CompanyNameTerm t) = ("company-name", unpack t)
toQueryItem (CurrentCompanyTerm b) = ("current-company", map toLower $ show b)
toQueryItem (TitleTerm t) = ("title", unpack t)
toQueryItem (CurrentTitleTerm b) = ("current-title", map toLower $ show b)
toQueryItem (SchoolNameTerm t) = ("school-name", unpack t)
toQueryItem (CurrentSchoolTerm b) = ("current-school", map toLower $ show b)
toQueryItem (CountryCodeTerm t) = ("country-code", unpack t)
toQueryItem (PostalCodeTerm t) = ("postal-code", unpack t)
toQueryItem (DistanceTerm i) = ("distance", show i)

data PeopleFieldSelector = IdSelector
                         | FirstNameSelector
                         | LastNameSelector
                         | HeadlineSelector
                         | PictureUrlSelector
                         deriving (Eq, Show)

toSelectorPiece :: PeopleFieldSelector -> String
toSelectorPiece IdSelector = "id"
toSelectorPiece FirstNameSelector = "first-name"
toSelectorPiece LastNameSelector = "last-name"
toSelectorPiece HeadlineSelector = "headline"
toSelectorPiece PictureUrlSelector = "picture-url"

toSelectorString :: [PeopleFieldSelector] -> String
toSelectorString = (":(people:("++) . (++"),num-results)") . intercalate "," . map toSelectorPiece

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
              , firstName :: Maybe Text
              , lastName :: Maybe Text
              , headline :: Maybe Text
              , pictureUrl :: Maybe Text
              } deriving (Show)
parsePerson :: MonadThrow m => Sink Event m (Maybe Person)
parsePerson = tagNoAttr "person" $ Person
              <$> (force "person must include an id" $ tagNoAttr "id" content)
              <*> tagNoAttr "first-name" content
              <*> tagNoAttr "last-name" content
              <*> tagNoAttr "headline" content
              <*> tagNoAttr "picture-url" content
