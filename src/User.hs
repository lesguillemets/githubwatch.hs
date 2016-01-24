{-# LANGUAGE OverloadedStrings #-}
module User (User) where
import Data.Aeson (FromJSON, decode, (.:))
import Control.Applicative
import qualified Data.Aeson as Ae

data User = User {
    _login :: String,
    _id :: Int,
    _avatarUrl :: String,
    _gravatarId :: String,
    _url :: String,
    _htmlUrl :: String,
    _followersUrl :: String,
    _followingUrl :: String,
    _gistsUrl :: String,
    _starredUrl :: String,
    _subscriptionsUrl :: String,
    _organizationsUrl :: String,
    _reposUrl :: String,
    _eventsUrl :: String,
    _receivedEventsUrl :: String,
    _type :: String,
    _siteAdmin :: Bool
} deriving (Show, Eq)

instance FromJSON User where
    parseJSON (Ae.Object v) =
        User <$> v .: "login"
             <*> v .: "id"
             <*> v .: "avatar_url"
             <*> v .: "gravatar_id"
             <*> v .: "url"
             <*> v .: "html_url"
             <*> v .: "followers_url"
             <*> v .: "following_url"
             <*> v .: "gists_url"
             <*> v .: "starred_url"
             <*> v .: "subscriptions_url"
             <*> v .: "organizations_url"
             <*> v .: "repos_url"
             <*> v .: "events_url"
             <*> v .: "received_events_url"
             <*> v .: "type"
             <*> v .: "site_admin"
    parseJSON _ = error "fail at user"
