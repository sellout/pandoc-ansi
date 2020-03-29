# ANSI writers for Pandoc

This adds a family of writers to Pandoc that provide output suitable for a
terminal. The intent is to make it possible to produce errors (and other output)
that is richer than text when possible, and also structured (e.g., lists,
paragraphs, code blocks).

The API is basically a family of writers `writeAnsi <fullAnsi> <vteLinks>`. The
documentation on `writeAnsi'` describes which `WriterOptions` and `Extensions`
are observed by the ANSI writers. In general, this tries to follow the Markdown
/ plain text formatting support in Pandoc, but adds ANSI colors and other
effects, as well as the option of setting the title of the terminal window.

This early version is still support for some features:
- tables
- notes
- refs

There are other components that would enhance this, but don't exist yet
- a layer focused on error processing
- some way to get convert `Outputable.SDoc` to Pandoc (or at least DocLayout)
