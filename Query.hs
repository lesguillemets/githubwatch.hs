module Query where
import Data.ByteString as B

data Order = Asc | Desc
instance Show Order where
    show Asc = "asc"
    show Desc = "desc"

data Sort = Stars | Forks | Updated
instance Show Sort where
    show Stars = "stars"
    show Forks = "forks"
    show Updated = "updated"

data Query = Query {
        _query :: B.ByteString, _sort :: Sort, _ord :: Order
        }
