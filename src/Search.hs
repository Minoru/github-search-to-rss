module Search (
  search
  ) where

import GitHub (github')
import GitHub.Endpoints.Search
import GitHub.Data.Request (FetchCount)
import qualified Data.Text as T

search :: T.Text -> IO (Either Error (SearchResult Issue))
search query = github' $ searchIssuesR query (FetchAtLeast 1)
