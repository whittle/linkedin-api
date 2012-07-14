{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings #-}
module Data.API.LinkedIn.Profile
       ( ProfileQuery(..)
       , ProfileResult(..)
       , CurrentShare(..)
       , ShareVisibility(..)
       , ShareSource(..)
       , ShareAuthor(..)
       , SiteStandardProfileRequest(..)
       ) where

import Data.API.LinkedIn.Query
import Data.API.LinkedIn.QueryResponsePair
import Data.API.LinkedIn.Response
import Network.API.LinkedIn --temp
import Network.API.ShimToken --temp
import Text.XML.Stream.Parse.Skip --temp

import Prelude hiding (concat)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import Data.Conduit (MonadThrow, Sink)
import Data.Default (Default(..))
import Data.Foldable (concat)
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
                     , profileNetwork :: Maybe ProfileNetwork
                     , numConnections :: Maybe Integer
                     , numConnectionsCapped :: Maybe Bool
                     , profileSummary :: Maybe Text
                     , profileSpecialties :: Maybe Text
                     , proposalComments :: Maybe Text
                     , profileAssociations :: Maybe Text
                     , profileHonors :: Maybe Text
                     , profileInterests :: Maybe Text
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
              <*> parseProfileNetwork
              <*> (fmap (fmap (read . unpack)) $ tagNoAttr (toName NumConnectionsS) content)
              <*> (fmap (fmap ("true"==)) $ tagNoAttr (toName NumConnectionsCappedS) content)
              <*> (fmap join $ tagNoAttr (toName SummaryS) contentMaybe)
              <*> (fmap join $ tagNoAttr (toName SpecialtiesS) contentMaybe)
              <*> (fmap join $ tagNoAttr (toName ProposalCommentsS) contentMaybe)
              <*> (fmap join $ tagNoAttr (toName AssociationsS) contentMaybe)
              <*> (fmap join $ tagNoAttr (toName HonorsS) contentMaybe)
              <*> (fmap join $ tagNoAttr (toName InterestsS) contentMaybe)
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

data ProfileNetwork = ProfileNetwork
                      { networkStats :: Maybe NetworkStats
                      , networkUpdates :: NetworkUpdates
                      } deriving (Show)
parseProfileNetwork :: MonadThrow m => Sink Event m (Maybe ProfileNetwork)
parseProfileNetwork = tagNoAttr (toName NetworkS) $ ProfileNetwork
                      <$> parseNetworkStats
                      <*> force "network must contain updates" parseNetworkUpdates

data NetworkStats = NetworkStats
                    { networkStatsTotal :: Integer
                    , networkStatsProperties :: [Property]
                    } deriving (Show)
parseNetworkStats :: MonadThrow m => Sink Event m (Maybe NetworkStats)
parseNetworkStats = tagName "network-stats" (requireAttr "total") $ \t -> fmap (NetworkStats (read $ unpack t)) $ many parseProperty

data Property = Property
                { propertyKey :: Text
                , propertyValue :: Text
                } deriving (Show)
parseProperty :: MonadThrow m => Sink Event m (Maybe Property)
parseProperty = tagName "property" (requireAttr "key") $ \k -> fmap (Property k) content

data NetworkUpdates = NetworkUpdates
                      { networkUpdatesTotal :: Integer
                      , networkUpdatesCount :: Maybe Integer
                      , networkUpdatesStart :: Maybe Integer
                      , allNetworkUpdates :: [NetworkUpdate]
                      } deriving (Show)
parseNetworkUpdates :: MonadThrow m => Sink Event m (Maybe NetworkUpdates)
parseNetworkUpdates = tagName "updates" tcsAttr $ \(t, c, s) -> NetworkUpdates t c s <$> many parseNetworkUpdate
  where tcsAttr = (,,) <$> reqIntAttr "total" <*> optIntAttr "count" <*> optIntAttr "start"
        reqIntAttr = fmap (read . unpack) . requireAttr
        optIntAttr = fmap (fmap (read . unpack)) . optionalAttr

data NetworkUpdate = NetworkUpdate
                     { updateTimestamp :: Integer
                     , updateKey :: Text
                     , updateType :: Text
                     , updateContent :: () -- incomplete
                     , updateIsCommentable :: Bool
                     , updateComments :: () -- incomplete
                     , updatedFields :: [Text]
                     , updateisLikable :: Bool
                     , updateIsLiked :: Maybe Bool
                     , updateNumLikes :: Maybe Integer
                     } deriving (Show)
parseNetworkUpdate :: MonadThrow m => Sink Event m (Maybe NetworkUpdate)
parseNetworkUpdate = tagNoAttr "update" $ NetworkUpdate
                     <$> (fmap (read . unpack) . force "update must contain timestamp" $ tagNoAttr "timestamp" content)
                     <*> (force "update must contain update-key" $ tagNoAttr "update-key" content)
                     <*> (force "update must contain update-type" $ tagNoAttr "update-type" content)
                     <*> (skipTag "update-content" >> return ())
                     <*> (fmap ("true"==) . force "update must contain is-commentable" $ tagNoAttr "is-commentable" content)
                     <*> (skipTag "update-comments" >> return ())
                     <*> fields
                     <*> (fmap ("true"==) . force "update must contain is-likable" $ tagNoAttr "is-likable" content)
                     <*> (fmap (fmap ("true"==)) $ tagNoAttr "is-liked" content)
                     <*> (fmap (fmap (read . unpack)) $ tagNoAttr "num-likes" content)
  where fields = fmap concat . tagName "updated-fields" ignoreAttrs
                 . const . many . tagNoAttr "update-field"
                 . force "update-field must contain name"
                 $ tagNoAttr "name" content

data ProfileFieldSelector = IdS | FirstNameS | LastNameS | MaidenNameS
                          | FormattedNameS | PhoneticFirstNameS
                          | PhoneticLastNameS | FormattedPhoneticNameS
                          | HeadlineS | IndustryS | DistanceS
                          | LastModifiedTimestampS | CurrentShareS | NetworkS
                          | NumConnectionsS | NumConnectionsCappedS | SummaryS
                          | SpecialtiesS | ProposalCommentsS | AssociationsS
                          | HonorsS | InterestsS | PositionsS | PublicationsS
                          | PatentsS | LanguagesS | SkillsS | CertificationsS
                          | EducationsS | CoursesS | VolunteerS
                          | ThreeCurrentPositionsS | ThreePastPositionsS
                          | NumRecommendersS | RecommendationsReceivedS
                          | PhoneNumbersS | ImAccountsS | TwitterAccountsS
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
                           , NumConnectionsS, NumConnectionsCappedS, SummaryS
                           , SpecialtiesS, ProposalCommentsS, AssociationsS
                           , HonorsS, InterestsS
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
