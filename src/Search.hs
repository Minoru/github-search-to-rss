module Search (
  search
  ) where

import GitHub.Endpoints.Search
import qualified Data.Text as T

search :: T.Text -> IO (Either Error (SearchResult Issue))
search query = searchIssues query
