{-# language OverloadedStrings, RecordWildCards #-}

-- | A family of D.Writers for printing to an ANSI terminal.
module Text.Pandoc.Writers.Ansi where

import Data.Functor.Identity (Identity(..))
import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI hiding (setSGR, setTitle)
import qualified System.Console.ANSI.Helpers as ANSI
import qualified System.Console.ANSI.VTE as ANSI
import System.IO (Handle)
import qualified Text.DocLayout as D
import qualified Text.Pandoc.Class as D
import qualified Text.Pandoc.Definition as D
import qualified Text.Pandoc.Extensions as D
import qualified Text.Pandoc.Options as D
import qualified Text.Pandoc.Shared as D
import qualified Text.Pandoc.Walk as D
import qualified Text.Pandoc.Writers as D
import qualified Text.Pandoc.Writers.Shared as D

-- | Prepends the ANSI `D.TextWriter`s onto an existing list of `D.Writer`s.
addWriters :: D.PandocMonad m => [(T.Text, D.Writer m)] -> [(T.Text, D.Writer m)]
addWriters =
  ([ ("ansi", D.TextWriter $ writeAnsi True False),
     ("ansi-color", D.TextWriter $ writeAnsi False False),
     ("vte", D.TextWriter $ writeAnsi True True)
   ]
     <>)

-- | Tries to return a `D.Writer` that takes advantage of the available ANSI
--   features. If it can't it uses the fallback `D.Writer` that was provided
--  (it's probably a good idea to use something that is readable from a
--   terminal, like `D.writePlain` or `D.writeMarkdown`.
--
--  __TODO__: Currently, if there's no unemulated ANSI support, we just use the
--            fallback, but ideally we could still return something
--           `D.Writer`-like that sent directly to `IO`.
selectAnsiWriter :: D.PandocMonad m => D.Writer m -> Bool -> Handle -> IO (D.Writer m)
selectAnsiWriter fallback vteLinks =
  fmap
    ( \case
        ANSI.NonEmulated -> D.TextWriter (writeAnsi True vteLinks)
        _ -> fallback
    )
    . ANSI.discoverAnsiSupport

-- data Accumulators = Accumulators { notes :: [[Blocks]], refs :: [(Text, Target, Attr)] }

-- | This makes this into a family of `D.TextWriter`, but better to use
--  `writeAnsi'` directly, as it's simply better constrained.
writeAnsi :: Applicative m => Bool -> Bool -> D.WriterOptions -> D.Pandoc -> m T.Text
writeAnsi full vteLinks opts = pure . writeAnsi' full vteLinks opts

-- | Write to an ANSI-compatible terminal, taking advantage of colors, window size, etc.
--
--   There is not yet a good way to inform this with details of the terminal's capability. We
--   currently assume full ANSI support _and_ VTE links.
--
--   Here are the `D.WriterOptions` that affect this `D.Writer`
--  [`D.writerColumns`] the terminal width
--  [`D.writerExtensions`] some extensions are interpreted, see below
--  [`D.writerPreferAscii`] for example, avoid box-drawing characters for tables, __NB__: this will
--                         /not/ convert non-ASCII characters within text literals.
--  [`D.writerTabStop`] the usual
--  [`D.writerWrapText`] the usual
--
--   Here are the (ostensibly Markdown) `D.Extensions` that affect this `D.Writer`
--  [`D.Ext_autolink_bare_uris`] use VTE links even on bare URIs
--  [`D.Ext_definition_lists`] ?
--  [`D.Ext_compact_definition_lists`] ?
--  [`D.Ext_gutenberg`] in cases where we don't have better markup (unlike `D.Emph`), use the Project Gutenberg style
--  [`D.Ext_hard_line_breaks`] whether or not to include literal line breaks
--  [`D.Ext_ignore_line_breaks`] eliminate literal newlines within paragraphs
--  [`D.Ext_smart`] this overrides `D.writerPreferAscii` for things like quotes and ellipses
--
--  __TODO__: This currently uses doclayout for pretty-printing, but AFAICT,
--            that doesn't support zero-width output (for ANSI escapes), so
--            this likely gets various line-lengths wrong.
writeAnsi' :: Bool -> Bool -> D.WriterOptions -> D.Pandoc -> T.Text
writeAnsi' fullAnsi vteLinks D.WriterOptions {..} (D.Pandoc meta blocks) =
  let meta' = runIdentity $ D.metaToContext' (pure . blocksToAnsi) (pure . inlinesToAnsi) meta
   in D.render Nothing
      $ (if fullAnsi
           then maybe D.empty (nonprinting . ANSI.setTitle ANSI.ansiString . T.unpack) (D.getField "title" meta')
           else D.empty)
        <> blocksToAnsi blocks
  where
    -- This is currently a lie, but a hint for us
    nonprinting :: T.Text -> D.Doc T.Text
    nonprinting = D.literal
    lineBreakToSpace :: D.Inline -> D.Inline
    lineBreakToSpace = \case
      D.LineBreak -> D.Space
      D.SoftBreak -> D.Space
      x -> x
    blocksToAnsi = foldMap blockToAnsi
    blockToAnsi :: D.Block -> D.Doc T.Text
    blockToAnsi = \case
      D.BlockQuote blks ->
        (D.prefixed "  " $ blocksToAnsi blks) <> D.blankline
      D.BulletList items ->
        D.vsep $
          fmap
            ( D.hang writerTabStop (D.literal (T.pack (if writerPreferAscii then "-" else "•") <> T.pack (replicate (writerTabStop - 1) ' ')))
              . blocksToAnsi
            )
            items
      D.CodeBlock (_, _, _) str ->
        nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetSwapForegroundBackground True])
          <> D.literal str
          <> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetSwapForegroundBackground False])
          <> D.blankline
      D.DefinitionList items ->
        foldMap
          (\(label, defs) -> do
              let label' = blockToAnsi (D.Plain label)
                  defs' = fmap blockToAnsi <$> defs
               in if D.extensionEnabled D.Ext_definition_lists writerExtensions
                then do
                  let leader  = "   "
                  let sps = case writerTabStop - 3 of
                        n | n > 0 -> D.literal $ T.replicate n " "
                        _ -> D.literal " "
                  let isTight = case defs of
                        ((D.Plain _ : _): _) -> True
                        _ -> False
                  if D.extensionEnabled D.Ext_compact_definition_lists writerExtensions
                    then
                      D.nowrap label'
                        <> D.cr
                        <> (D.vcat $ fmap (\d -> D.hang writerTabStop (leader <> sps) $ D.vcat d <> D.cr) defs')
                        <> D.cr
                    else
                      let contents = (if isTight then D.vcat else D.vsep) $ fmap
                                     (\d -> D.hang writerTabStop (leader <> sps) $ D.vcat d)
                                     defs'
                       in D.blankline <> D.nowrap label' D.$$
                        (if isTight then D.empty else D.blankline) <> contents <> D.blankline
                else
                  D.nowrap (D.chomp label' <> D.literal "  " <> D.cr) <> D.vsep (fmap D.vsep defs') <> D.blankline
          )
          items
          <> D.blankline
      D.Div _ blks -> blocksToAnsi blks
      D.Header level _ inlines ->
        if D.extensionEnabled D.Ext_gutenberg writerExtensions
        then
          case level of
            1 -> D.blanklines 3 <> contents (D.capitalize inlines) <> D.blanklines 2
            2 -> D.blanklines 2 <> contents inlines <> D.blankline
            _ -> contents inlines <> D.blankline
        else
          contents inlines <> D.blankline
        where
          contents = inlinesToAnsi . D.walk lineBreakToSpace
      D.HorizontalRule ->
        D.blankline
          <> D.literal (T.pack . replicate writerColumns $ if writerPreferAscii then '-' else '─')
          <> D.blankline
      D.LineBlock lns ->
        D.vcat (fmap (D.hang 2 (D.literal $ if writerPreferAscii then "| " else "│ ") . inlinesToAnsi) lns)
          <> D.blankline
      D.Null -> D.empty
      D.OrderedList (start, sty, delim) items ->
        let start' = if D.extensionEnabled D.Ext_startnum writerExtensions then start else 1
            markers = D.orderedListMarkers (start', sty, delim)
        in D.vsep $
          zipWith
            (\m -> D.hang writerTabStop (D.literal (m <> " ")) . blocksToAnsi)
            markers
            items
      D.Para ils -> blockToAnsi $ D.Plain ils
      D.Plain ils -> inlinesToAnsi ils <> D.cr
      D.RawBlock _ str -> D.literal str <> D.blankline
      -- __TODO__: Implement tables
      D.Table _ _ _ _ _ -> D.literal "[TABLE]"
    inlinesToAnsi :: [D.Inline] -> D.Doc T.Text
    inlinesToAnsi = foldMap inlineToAnsi
    inlineToAnsi :: D.Inline -> D.Doc T.Text
    inlineToAnsi = \case
      D.Span _ ils -> inlinesToAnsi ils
      D.Emph ils -> do
        let contents = inlinesToAnsi ils
         in if fullAnsi
           then nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetItalicized True]) <> contents <> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetItalicized False])
           else if D.extensionEnabled D.Ext_gutenberg writerExtensions
             then D.literal "_" <> contents <> D.literal "_"
             else contents
      D.Strong ils ->
        if fullAnsi
        then nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetConsoleIntensity ANSI.BoldIntensity]) <> inlinesToAnsi ils <> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetConsoleIntensity ANSI.NormalIntensity])
        else if D.extensionEnabled D.Ext_gutenberg writerExtensions
             then inlinesToAnsi (D.capitalize ils)
             else inlinesToAnsi ils
      D.Strikeout ils ->
        if fullAnsi
          then nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetConsoleIntensity ANSI.FaintIntensity]) <> inlinesToAnsi ils <> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetConsoleIntensity ANSI.NormalIntensity])
          else inlinesToAnsi ils
      D.Superscript ils ->
        let rendered = D.render Nothing $ inlinesToAnsi ils
         in D.literal . maybe ("^(" <> rendered <> ")") T.pack . traverse D.toSuperscript
           $ T.unpack rendered
      D.Subscript ils ->
        let rendered = D.render Nothing $ inlinesToAnsi ils
         in D.literal . maybe ("_(" <> rendered <> ")") T.pack . traverse D.toSubscript
           $ T.unpack rendered
      D.SmallCaps ils -> inlinesToAnsi $ D.capitalize ils
      D.Quoted D.SingleQuote ils ->
        let contents = inlinesToAnsi ils
         in if D.extensionEnabled D.Ext_smart writerExtensions
            then D.literal "‘" <> contents <> D.literal "’"
            else D.literal "'" <> contents <> D.literal "'"
      D.Quoted D.DoubleQuote ils ->
        let contents = inlinesToAnsi ils
         in if D.extensionEnabled D.Ext_smart writerExtensions
            then D.literal "“" <> contents <> D.literal "”"
            else D.literal "\"" <> contents <> D.literal "\""
      D.Code _ str -> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetSwapForegroundBackground True]) <> D.literal str <> nonprinting (ANSI.setSGR ANSI.ansiString [ANSI.SetSwapForegroundBackground False])
      D.Str str -> D.literal str
      D.Math _ str -> D.literal str
      D.RawInline _ str -> D.literal str
      D.LineBreak -> if D.extensionEnabled D.Ext_hard_line_breaks writerExtensions then D.cr else D.space
      D.Space -> D.space
      D.SoftBreak -> case writerWrapText of
        D.WrapNone -> D.space
        D.WrapAuto -> D.space
        D.WrapPreserve -> D.cr
      D.Cite _ ils -> inlinesToAnsi ils
      D.Link _ txt (src, _) -> do
        let content = inlinesToAnsi txt 
         in if vteLinks
           then nonprinting (ANSI.setLink mempty src) <> content <> nonprinting ANSI.unsetLink
           else content <> D.literal (" <" <> src <> ">")
      D.Image _ _ (src, _) ->
        D.literal "["
          <> (if vteLinks
               then nonprinting (ANSI.setLink mempty src) <> D.literal src <> nonprinting ANSI.unsetLink
               else D.literal src)
          <> D.literal "]"
      D.Note _blocks -> D.empty
        -- TODO: collect footnotes, and include reference here
        -- "[" <> inlinesToAnsi ils <> "]"
    -- notesToAnsi :: [[D.Block]] -> D.Doc T.Text
    -- notesToAnsi = D.vsep . zipWith noteToAnsi [1..]
    -- noteToAnsi :: Natural -> [D.Block] -> State Accumulators (D.Doc T.Text)
    -- noteToAnsi n blocks = do
    --  content <- blocksToAnsi blocks
    --  let index = tshow n
    --      marker = literal "[" <> index <> literal "]"
    --      markerSize = 4 + offset index
    --      spacer = case D.writerTabStop opts - markerSize of
    --                  n | n > 0  -> literal $ T.replicate n " "
    --                  _ -> literal " "
    --  pure $ marker <> spacer <> contents
