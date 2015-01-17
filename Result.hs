{-# LANGUAGE OverloadedStrings #-}
module Result where
import Control.Applicative
import Data.Aeson (FromJSON, decode, (.:))
import qualified Data.Aeson as Ae
import GHC.Exts (sortWith)
import qualified User as U

type DateTime = String

data Result = Result {
    _totalCount :: Int,
    _incompleteResults :: Bool,
    _items :: [Item]
} deriving (Show)

emptyResult :: Result
emptyResult = Result 0 False []

mergeResults :: Result -> Result -> Result
mergeResults (Result c0 b0 i0) (Result c1 b1 i1) =
        Result (c0+c1) (b0&&b1) (i0++i1)


instance FromJSON Result where
    parseJSON (Ae.Object v) =
        Result <$> v .: "total_count"
               <*> v .: "incomplete_results"
               <*> v .: "items"
    parseJSON _ = error "fail at Result"

printRecents :: Int -> Result -> IO()
printRecents n = mapM_ (putStrLn . showInfo) . take n . reverse . sortWith _updatedAt . _items

showInfo :: Item -> String
showInfo i = unlines [
        unwords [_name i, "  --", _description i],
        _htmlUrl i,
        "      " ++ _updatedAt i
    ]

data Item = Item {
    _id :: Integer,
    _name :: String,
    _fullName :: String,
    _private :: Bool,
    _owner :: U.User,
    _htmlUrl :: String,
    _description :: String,
    _fork :: Bool,
    _url :: String,
    _forksUrl :: String,
    _keysurl :: String,
    _collaboratorsUrl :: String,
    _teamsUrl :: String,
    _hooksUrl :: String,
    _issueEventsUrl :: String,
    _eventsUrl :: String,
    _assigneesUrl :: String,
    _branchesUrl :: String,
    _tagsUrl :: String,
    _blobsUrl :: String,
    _gitTagsUrl :: String,
    _gitRefsUrl :: String,
    _treesUrl :: String,
    _statusesUrl :: String,
    _languagesUrl :: String,
    _stargazersUrl :: String,
    _contributorsUrl :: String,
    _subscribersUrl :: String,
    _subscriptionUrl :: String,
    _commitsUrl :: String,
    _gitCommitsUrl :: String,
    _commentsUrl :: String,
    _issueCommentUrl :: String,
    _contentsUrl :: String,
    _compareUrl :: String,
    _mergesUrl :: String,
    _archiveUrl :: String,
    _downloadsUrl :: String,
    _issuesUrl :: String,
    _pullsUrl :: String,
    _milestonesUrl :: String,
    _notificationsUrl :: String,
    _labelsUrl :: String,
    _releasesUrl :: String,
    _createdAt :: DateTime,
    _updatedAt :: DateTime,
    _pushedAt :: DateTime,
    _gitUrl :: String,
    _sshUrl :: String,
    _cloneUrl :: String,
    _svnUrl :: String,
    _homepage :: Maybe String,
    _size :: Integer,
    _stargazersCount :: Int,
    _watchersCount :: Int,
    _language :: Maybe String,
    _hasIssues :: Bool,
    _hasDownloads :: Bool,
    _hasWiki :: Bool,
    _hasPages :: Bool,
    _forksCount :: Int,
    _mirrorUrl :: Maybe String,
    _openIssuesCount :: Int,
    _forks :: Int,
    _openIssues :: Int,
    _watchers :: Int,
    _defaultBranch :: String,
    _score :: Double
} deriving (Show, Eq)

instance FromJSON Item where
    parseJSON (Ae.Object v) =
        Item <$> v .: "id"
             <*> v .: "name"
             <*> v .: "full_name"
             <*> v .: "private"
             <*> v .: "owner"
             <*> v .: "html_url"
             <*> v .: "description"
             <*> v .: "fork"
             <*> v .: "url"
             <*> v .: "forks_url"
             <*> v .: "keys_url"
             <*> v .: "collaborators_url"
             <*> v .: "teams_url"
             <*> v .: "hooks_url"
             <*> v .: "issue_events_url"
             <*> v .: "events_url"
             <*> v .: "assignees_url"
             <*> v .: "branches_url"
             <*> v .: "tags_url"
             <*> v .: "blobs_url"
             <*> v .: "git_tags_url"
             <*> v .: "git_refs_url"
             <*> v .: "trees_url"
             <*> v .: "statuses_url"
             <*> v .: "languages_url"
             <*> v .: "stargazers_url"
             <*> v .: "contributors_url"
             <*> v .: "subscribers_url"
             <*> v .: "subscription_url"
             <*> v .: "commits_url"
             <*> v .: "git_commits_url"
             <*> v .: "comments_url"
             <*> v .: "issue_comment_url"
             <*> v .: "contents_url"
             <*> v .: "compare_url"
             <*> v .: "merges_url"
             <*> v .: "archive_url"
             <*> v .: "downloads_url"
             <*> v .: "issues_url"
             <*> v .: "pulls_url"
             <*> v .: "milestones_url"
             <*> v .: "notifications_url"
             <*> v .: "labels_url"
             <*> v .: "releases_url"
             <*> v .: "created_at"
             <*> v .: "updated_at"
             <*> v .: "pushed_at"
             <*> v .: "git_url"
             <*> v .: "ssh_url"
             <*> v .: "clone_url"
             <*> v .: "svn_url"
             <*> v .: "homepage"
             <*> v .: "size"
             <*> v .: "stargazers_count"
             <*> v .: "watchers_count"
             <*> v .: "language"
             <*> v .: "has_issues"
             <*> v .: "has_downloads"
             <*> v .: "has_wiki"
             <*> v .: "has_pages"
             <*> v .: "forks_count"
             <*> v .: "mirror_url"
             <*> v .: "open_issues_count"
             <*> v .: "forks"
             <*> v .: "open_issues"
             <*> v .: "watchers"
             <*> v .: "default_branch"
             <*> v .: "score"
    parseJSON _ = error "fail at Item"
