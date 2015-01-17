{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (decode)
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Network.HTTP.Conduit as HC
import Control.Monad.IO.Class (liftIO)

import qualified Query as Q
import qualified Result as R


githubBaseURL = "https://api.github.com"
githubSearchEndpoint = "/search/repositories"
Just apiTarget = HC.parseUrl $ githubBaseURL ++ githubSearchEndpoint

searchGithub :: Q.Query -> IO (Maybe (HC.Response L.ByteString))
searchGithub q = HC.withManager $ \ m -> do
            let postRes = HC.setQueryString [
                            ("q", Just (Q._query q)),
                            ("sort", Just (BC.pack . show . Q._sort $ q)),
                            ("order", Just (BC.pack . show . Q._ord $ q))
                            ] $ apiTarget {
                    HC.method = "GET",
                    HC.requestHeaders = [
                        ("user-agent", "Haskell-HTTP-Conduit")
                        ]
            }
            response <- HC.httpLbs postRes m
            return (Just response)

main = do
    r0 <- searchGithub (Q.Query "vim+colorscheme" Q.Updated Q.Desc)
    case r0 of
        Nothing -> print "Hi"
        Just res -> do
            let results =  (decode $ HC.responseBody res) :: (Maybe R.Result)
            case results of
                Nothing -> print "oops"
                Just reps -> R.printRecents 10 reps
