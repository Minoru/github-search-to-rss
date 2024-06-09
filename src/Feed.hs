{-# LANGUAGE OverloadedStrings #-}

module Feed (
  toAtomFeed
  ) where

import GitHub.Data (SearchResult, Issue(..), getUrl)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldMap)
import Data.Time.RFC3339 (formatTimeRFC3339)
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Data.Vector (toList)
import Text.XML (def, renderText)
import Data.XML.Types as XML
import Text.XML as XMLC
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

toAtomFeed :: TimeZone -> T.Text -> SearchResult Issue -> T.Text
toAtomFeed tz query results =
  let
    entries = foldMap toList results
    atomEntries = map (issueToAtomEntry tz) entries
  in fromMaybe T.empty (renderFeed $ createFeed query atomEntries)

issueToAtomEntry :: TimeZone -> Issue -> Atom.Entry
issueToAtomEntry tz issue = entry
  where
    uri :: Atom.URI
    -- Atom entry will link to HTML version of the issue if it's available,
    -- otherwise it'll link to the API entry for that issue
    uri = getUrl $ fromMaybe (issueUrl issue) (issueHtmlUrl issue)

    title :: Atom.TextContent
    title = Atom.TextString $ issueTitle issue

    updated :: Atom.Date
    updated = formatTimeRFC3339 $ utcToZonedTime tz $ issueUpdatedAt issue

    basicEntry :: Atom.Entry
    basicEntry = Atom.nullEntry uri title updated

    content :: Atom.EntryContent
    content = Atom.TextContent $ fromMaybe T.empty (issueBody issue)

    entry :: Atom.Entry
    entry = basicEntry
      {
        Atom.entryContent = Just content
      , Atom.entryLinks = [ Atom.nullLink uri ]
      }

createFeed :: T.Text -> [Atom.Entry] -> Atom.Feed
createFeed query entries = feed
  where
    uri :: Atom.URI
    uri = T.concat [ "https://debiania.in.ua/github-search-to-rss/", query ]

    title :: Atom.TextContent
    title = Atom.TextString $ T.concat [ "GitHub search results for \"", query, "\""]

    updated :: Atom.Date
    updated =
      case entries of
        (entry:_) -> Atom.entryUpdated entry
        _ -> "1970-01-01T00:00:00+00:00"

    basicFeed :: Atom.Feed
    basicFeed = Atom.nullFeed uri title updated

    feed :: Atom.Feed
    feed = basicFeed { Atom.feedEntries = entries }

renderFeed :: Atom.Feed -> Maybe T.Text
renderFeed feed =
  fmap (TL.toStrict . renderText def) $
  elementToDoc $
  Export.xmlFeed $
  feed

elementToDoc :: XML.Element -> Maybe XMLC.Document
elementToDoc element =
  either (const Nothing) Just $
  fromXMLDocument $
  XML.Document (Prologue [] Nothing []) element []

{-
Issue {
  issueClosedAt = Nothing,
  issueUpdatedAt = 2018-03-21 11:30:56 UTC,
  issueEventsUrl = URL "https://api.github.com/repos/graysky2/profile-cleaner/issues/27/events",
  issueHtmlUrl = Just (URL "https://github.com/graysky2/profile-cleaner/issues/27"),
  issueClosedBy = Nothing,
  issueLabels = [],
  issueNumber = 27,
  issueAssignees = [],
  issueUser = SimpleUser {
    simpleUserId = Id 200897,
    simpleUserLogin = N "Dr-Terrible",
    simpleUserAvatarUrl = URL "https://avatars1.githubusercontent.com/u/200897?v=4",
    simpleUserUrl = URL "https://api.github.com/users/Dr-Terrible"
    },
  issueTitle = "Support for Newsboat",
  issuePullRequest = Nothing,
  issueUrl = URL "https://api.github.com/repos/graysky2/profile-cleaner/issues/27",
  issueCreatedAt = 2018-03-21 11:29:44 UTC,
  issueBody = Just "Newsbeuter has been abandoned by upstream and has been forked as Newsboat: https://github.com/akrennmair/newsbeuter/blob/7c981f460d6c8c3690f140cbb279c277dc8f55fe/README.md\r\n\r\nThe profile is in `~/.local/share/newsboat` instead of `~/.local/share/newsbeuter`.\r\n\r\np.s.: Newsboat is maintaing compatibility with the original sqlite3 schemas from Newsbeuter",
  issueState = StateOpen,
  issueId = Id 307207604,
  issueComments = 0,
  issueMilestone = Nothing
}
-}
