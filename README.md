# github-search-to-rss

Converts results of GitHub issues search into an RSS feed (actually Atom).

**Don't mind the lack of updates!** This tool has been working fine for me since
2018.

It's meant to be used in conjunction with [the `exec:` feature of Newsboat RSS
reader][newsboat-exec]. Put `github-search-to-rss` binary into `~/.bin`, and add
the following to your `urls` file:

```
"exec:~/.bin/github-search-to-rss -- whatever interests you"
```

[newsboat-exec]: https://newsboat.org/releases/2.11.1/docs/newsboat.html#_scripts_and_filters_snownews_extensions
    "The Newsboat RSS Feedreader — Scripts and Filters (Snownews Extensions)"
