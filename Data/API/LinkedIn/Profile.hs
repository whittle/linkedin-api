{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings #-}
module Data.API.LinkedIn.Profile
       ( ProfileQuery
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response
import Network.API.LinkedIn --temp
import Network.API.ShimToken --temp

import Control.Applicative ((<$>), (<*>))
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.XML.Types (Event, Name(..))
import Text.XML.Stream.Parse

data ProfileQuery = ProfileQuery
                    { queryMemberId :: Text
                    , fieldSelectors :: [ProfileFieldSelector]
                    } deriving (Show)

instance Default ProfileQuery where
  def = ProfileQuery "" allProfileFieldSelectors

instance Query ProfileQuery where
  toPathSegments q = ["people"
                     , foldr (++) "" [ "id="
                                     , (unpack $ queryMemberId q)
                                     , ":"
                                     , (formatSelectors $ fieldSelectors q)
                                     ]
                     ]
  toQueryItems = const []

data ProfileResult = ProfileResult
                     { profileId :: Maybe Text
                     , firstName :: Maybe Text
                     , lastName :: Maybe Text
                     , maidenName :: Maybe Text
                     , formattedName :: Maybe Text
                     , phoneticFirstName :: Maybe Text
                     , phoneticLastName :: Maybe Text
                     , formattedPhoneticName :: Maybe Text
                     , headline :: Maybe Text
                     , industry :: Maybe Text
                     , distance :: Maybe Integer
                     , lastModifiedTimestamp :: Maybe Integer
                     , currentShare :: Maybe CurrentShare
                     , siteStandardProfileRequest :: Maybe SiteStandardProfileRequest
                     } deriving (Show)

instance Response ProfileResult where
  parsePage = tagNoAttr "person" $ ProfileResult
              <$> tagNoAttr (toName IdS) content
              <*> tagNoAttr (toName FirstNameS) content
              <*> tagNoAttr (toName LastNameS) content
              <*> tagNoAttr (toName MaidenNameS) content
              <*> tagNoAttr (toName FormattedNameS) content
              <*> tagNoAttr (toName PhoneticFirstNameS) content
              <*> tagNoAttr (toName PhoneticLastNameS) content
              <*> tagNoAttr (toName FormattedPhoneticNameS) content
              <*> tagNoAttr (toName HeadlineS) content
              <*> tagNoAttr (toName IndustryS) content
              <*> (fmap (fmap (read . unpack)) $ tagNoAttr (toName DistanceS) content)
              <*> (fmap (fmap (read . unpack)) $ tagNoAttr (toName LastModifiedTimestampS) content)
              <*> parseCurrentShare
              <*> parseSiteStandardProfileRequest

instance QueryResponsePair ProfileQuery ProfileResult

data SiteStandardProfileRequest = SiteStandardProfileRequest
                                  { siteStandardProfileRequestUrl :: Text
                                  } deriving (Show)
parseSiteStandardProfileRequest :: MonadThrow m
                                   => Sink Event m (Maybe SiteStandardProfileRequest)
parseSiteStandardProfileRequest = tagNoAttr "site-standard-profile-request" $ SiteStandardProfileRequest
                                  <$> (force "site-standard-profile-request must contain an url" $ tagNoAttr "url" content)

data CurrentShare = CurrentShare
                    { shareId :: Text
                    , shareTimestamp :: Integer
                    , shareVisibility :: ShareVisibility
                    , shareComment :: Text
                    , shareSource :: ShareSource
                    , shareAuthor :: ShareAuthor
                    } deriving (Show)
parseCurrentShare :: MonadThrow m => Sink Event m (Maybe CurrentShare)
parseCurrentShare = tagNoAttr (toName CurrentShareS) $ CurrentShare
                    <$> (force "current-share must contain an id" $ tagNoAttr "id" content)
                    <*> (fmap (read . unpack) . force "current-share must contain a timestamp" $ tagNoAttr "timestamp" content)
                    <*> force "current-share must contain visibility" parseShareVisibility
                    <*> (force "current-share must contain a comment" $ tagNoAttr "comment" content)
                    <*> force "current-share must contain a source" parseShareSource
                    <*> force "current-share must contain an author" parseShareAuthor

data ShareVisibility = ShareVisibility
                       { visibilityCode :: Text
                       } deriving (Show)
parseShareVisibility :: MonadThrow m => Sink Event m (Maybe ShareVisibility)
parseShareVisibility = tagNoAttr "visibility" $ ShareVisibility
                       <$> (force "visibility must contain a code" $ tagNoAttr "code" content)

data ShareSource = ShareSource
                   { sourceServiceProviderName :: Text
                   } deriving (Show)
parseShareSource :: MonadThrow m => Sink Event m (Maybe ShareSource)
parseShareSource = tagNoAttr "source" . fmap ShareSource . force "source must contain service-provider" . tagNoAttr "service-provider" . force "service-provider must contain name" $ tagNoAttr "name" content

data ShareAuthor = ShareAuthor
                   { authorId :: Text
                   , authorFirstName :: Text
                   , authorLastName :: Text
                   } deriving (Show)
parseShareAuthor :: MonadThrow m => Sink Event m (Maybe ShareAuthor)
parseShareAuthor = tagNoAttr "author" $ ShareAuthor
                   <$> (force "author must contain id" $ tagNoAttr "id" content)
                   <*> (force "author must contain first-name" $ tagNoAttr "first-name" content)
                   <*> (force "author must contain last-name" $ tagNoAttr "last-name" content)

data ProfileFieldSelector = IdS | FirstNameS | LastNameS | MaidenNameS
                          | FormattedNameS | PhoneticFirstNameS
                          | PhoneticLastNameS | FormattedPhoneticNameS
                          | HeadlineS | IndustryS | DistanceS
                          | LastModifiedTimestampS | CurrentShareS | NetworkS
                          | ConnectionsS | NumConnectionsS
                          | NumConnectionsCappedS | SummaryS | SpecialtiesS
                          | ProposalCommentsS | AssociationsS | HonorsS
                          | InterestsS | PositionsS | PublicationsS | PatentsS
                          | LanguagesS | SkillsS | CertificationsS | EducationsS
                          | CoursesS | VolunteerS | ThreeCurrentPositionsS
                          | ThreePastPositionsS | NumRecommendersS
                          | RecommendationsReceivedS | PhoneNumbersS
                          | ImAccountsS | TwitterAccountsS
                          | PrimaryTwitterAccountS | BoundAccountTypesS
                          | MfeedRssUrlS | FollowingS | JobBookMarksS
                          | GroupMembershipsS | SuggestionsS | DateOfBirthS
                          | MainAddressS | MemberUrlResourcesS | PictureUrlS
                          | SiteStandardProfileRequestS | PublicProfileUrlS
                          | RelatedProfileViewsS
                          deriving (Show)

allProfileFieldSelectors = [ IdS, FirstNameS, LastNameS, MaidenNameS
                           , FormattedNameS, PhoneticFirstNameS
                           , PhoneticLastNameS, FormattedPhoneticNameS
                           , HeadlineS, IndustryS, DistanceS
                           , LastModifiedTimestampS, CurrentShareS
                           , SiteStandardProfileRequestS]

fieldName :: ProfileFieldSelector -> String
fieldName IdS = "id"
fieldName FirstNameS = "first-name"
fieldName LastNameS = "last-name"
fieldName MaidenNameS = "maiden-name"
fieldName FormattedNameS = "formatted-name"
fieldName PhoneticFirstNameS = "phonetic-first-name"
fieldName PhoneticLastNameS = "phonetic-last-name"
fieldName FormattedPhoneticNameS = "formatted-phonetic-name"
fieldName HeadlineS = "headline"
-- fieldName = "location:(name)"
-- fieldName = "location:(country:(code))"
fieldName IndustryS = "industry"
fieldName DistanceS = "distance"
-- fieldName = "relation-to-viewer:(distance)"
fieldName LastModifiedTimestampS = "last-modified-timestamp"
fieldName CurrentShareS= "current-share"
fieldName NetworkS = "network"
fieldName ConnectionsS = "connections"
fieldName NumConnectionsS = "num-connections"
fieldName NumConnectionsCappedS = "num-connections-capped"
fieldName SummaryS = "summary"
fieldName SpecialtiesS = "specialties"
fieldName ProposalCommentsS = "proposal-comments"
fieldName AssociationsS = "associations"
fieldName HonorsS = "honors"
fieldName InterestsS = "interests"
fieldName PositionsS = "positions"
fieldName PublicationsS = "publications"
fieldName PatentsS = "patents"
fieldName LanguagesS = "languages"
fieldName SkillsS = "skills"
fieldName CertificationsS = "certifications"
fieldName EducationsS = "educations"
fieldName CoursesS = "courses"
fieldName VolunteerS = "volunteer"
fieldName ThreeCurrentPositionsS = "three-current-positions"
fieldName ThreePastPositionsS = "three-past-positions"
fieldName NumRecommendersS = "num-recommenders"
fieldName RecommendationsReceivedS = "recommendations-received"
fieldName PhoneNumbersS = "phone-numbers"
fieldName ImAccountsS = "im-accounts"
fieldName TwitterAccountsS = "twitter-accounts"
fieldName PrimaryTwitterAccountS = "primary-twitter-account"
fieldName BoundAccountTypesS = "bound-account-types"
fieldName MfeedRssUrlS = "mfeed-rss-url"
fieldName FollowingS = "following"
fieldName JobBookMarksS = "job-bookmarks"
fieldName GroupMembershipsS = "group-memberships"
fieldName SuggestionsS = "suggestions"
fieldName DateOfBirthS = "date-of-birth"
fieldName MainAddressS = "main-address"
fieldName MemberUrlResourcesS = "member-url-resources"
-- fieldName = "member-url-resources:(url)"
-- fieldName = "member-url-resources:(name)"
fieldName PictureUrlS = "picture-url"
fieldName SiteStandardProfileRequestS = "site-standard-profile-request"
-- fieldName = "api-standard-profile-request:(url)"
-- fieldName = "api-standard-profile-request:(headers)"
fieldName PublicProfileUrlS = "public-profile-url"
fieldName RelatedProfileViewsS= "related-profile-views"

formatSelectors :: [ProfileFieldSelector] -> String
formatSelectors ps = "(" ++ fields' ps ++ ")"
  where fields' = intercalate "," . map fieldName

toName :: ProfileFieldSelector -> Name
toName s = Name (pack $ fieldName s) Nothing Nothing
