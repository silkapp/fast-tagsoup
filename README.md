fast-tagsoup
============

Fast Haskell tagsoup parser.

Speeds of 20-200MB/sec were observed.

Works only with strict bytestrings.

This library is intended to be used in conjunction with the original <tt>tagsoup</tt> package:

<pre>
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast.Utf8Only
</pre>

Besides speed <tt>fast-tagsoup</tt> correctly handles HTML <tt>&lt;script&gt;</tt> and <tt>&lt;style&gt;</tt> tags and converts tags to lower case.
This fork purposefully removes support for parsing non-utf8 documents, to avoid dependency on text-icu. If you need to handle other encodings, use the original http://hackage.haskell.org/package/fast-tagsoup

This parser is used in production in <a href="http://bazqux.com">BazQux Reader</a> feeds and comments crawler.
