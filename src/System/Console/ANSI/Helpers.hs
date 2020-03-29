{-# language OverloadedStrings #-}

-- | Use the ol' RoF trick to eliminate a bunch of the duplication in the ansi-terminal API.
module System.Console.ANSI.Helpers where

import Data.String (IsString(..))
import System.Console.ANSI hiding (cursorUp, cursorDown, cursorForward, cursorBackward, cursorUpLine, cursorDownLine, setSGR, setTitle)
import System.IO (Handle, stdout)

data AnsiOps a =
  AnsiOps
    { -- | cursor movement by character
      cursorUp :: Int -> a,
      cursorDown :: Int -> a,
      cursorForward :: Int -> a,
      cursorBackward :: Int -> a,
      -- | cursor movement by line
      cursorUpLine :: Int -> a,
      cursorDownLine :: Int -> a,
      -- | directly changing cursor position
      -- | saving, restoring, and reporting cursor position
      -- | clearing parts of the screen
      -- | scrolling the screen
      -- | Select Graphic Rendition mode
      -- colors and other whizzy stuff
      -- | Commands: these will typically be applied on top of the current console SGR mode. An
      --   empty list of commands is equivalent to the list @[`Reset`]@. Commands are applied left
      --   to right.
      setSGR :: [SGR] -> a,
      -- | cursor visibility changes
      -- | changing the title
      -- | XTerm control sequence to set the Icon Name and Window Title.
      setTitle :: String -> a
    }

-- | Useful when you're working with multiple `Handle`s at once.
ansiIO :: AnsiOps (Handle -> IO ())
ansiIO =
  AnsiOps
    { cursorUp = flip hCursorUp,
      cursorDown = flip hCursorDown,
      cursorForward = flip hCursorForward,
      cursorBackward = flip hCursorBackward,
      cursorUpLine = flip hCursorUpLine,
      cursorDownLine = flip hCursorDownLine,
      setSGR = flip hSetSGR,
      setTitle = flip hSetTitle
    }

-- | Useful when you want to do everything on a single `Handle`.
ansiHandle :: Handle -> AnsiOps (IO ())
ansiHandle handle =
  AnsiOps
    { cursorUp = hCursorUp handle,
      cursorDown = hCursorDown handle,
      cursorForward = hCursorForward handle,
      cursorBackward = hCursorBackward handle,
      cursorUpLine = hCursorUpLine handle,
      cursorDownLine = hCursorDownLine handle,
      setSGR = hSetSGR handle,
      setTitle = hSetTitle handle
    }

-- | Trivial, but one of the options provided by ansi-terminal, so why not?
ansiStdout :: AnsiOps (IO ())
ansiStdout = ansiHandle stdout

-- | Builds a `String` that can be output later.
--
--  __NB__: This version has no effect on Windows, because the terminal isn't ANSI, and we can't
--          substitute the correct IO actions here.
ansiString :: IsString s => AnsiOps s
ansiString =
  AnsiOps
    { cursorUp = fromString . cursorUpCode,
      cursorDown = fromString . cursorDownCode,
      cursorForward = fromString . cursorForwardCode,
      cursorBackward = fromString . cursorBackwardCode,
      cursorUpLine = fromString . cursorUpLineCode,
      cursorDownLine = fromString . cursorDownLineCode,
      setSGR = fromString . setSGRCode,
      setTitle = fromString . setTitleCode
    }

data AnsiSupport
 = NoAnsi
 | EmulatedColors
 | FullEmulated
 | NonEmulated

-- | Returns `Nothing` if there is no support, otherwise `Just (supportsString, notJustColor)`.
--
--   It's very unclear from the documentation of ansi-terminal how these things
--   relate to each other, so the result here might not be correct. E.g., what's
--   the case if `hSupportsANSIWithoutEmulation` is `True`, but both other
--   ones are `False`? Right now, that means no support, even though it's been
--   indicated that there /is/ support.
discoverAnsiSupport :: Handle -> IO AnsiSupport
discoverAnsiSupport handle = do
  withoutEmulation <- hSupportsANSIWithoutEmulation handle
  case withoutEmulation of
    Just True -> pure NonEmulated
    _ -> do
      support <- hSupportsANSI handle
      if support
        then pure FullEmulated
          else do
            colors <- hSupportsANSIColor handle
            pure $ if colors then EmulatedColors else NoAnsi
