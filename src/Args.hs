module Args (
    Options(..)
  , getArgs
  ) where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Text as T

data Options = Options { query :: T.Text }

getArgs :: IO Options
getArgs = execParser p
  where
    p = info (options <**> helper)
      ( fullDesc
      <> progDesc "Converts GitHub search results into an RSS feed (actually Atom).")

options :: Parser Options
options = Options
  <$> fmap T.unwords (some (argument str (metavar "QUERY...")))
