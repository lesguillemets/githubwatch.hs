{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Aeson (decode)
import Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Network.HTTP.Conduit as HC

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

decodeResponse ::  HC.Response L.ByteString -> R.Result
decodeResponse  = fromMaybe R.emptyResult . decode . HC.responseBody

query0 :: Q.Query
query1 :: Q.Query
query0 = Q.Query "vim+colorscheme" Q.Updated Q.Desc
query1 = Q.Query "vim+color+scheme" Q.Updated Q.Desc

main = do
    r0 <- decodeResponse . fromMaybe undefined <$> searchGithub query0
    r1 <- decodeResponse . fromMaybe undefined <$> searchGithub query1
    let results = R.mergeResults r0 r1
    R.printRecents 10 results
