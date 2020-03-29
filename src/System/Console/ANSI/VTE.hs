{-# language OverloadedStrings #-}

-- | https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
module System.Console.ANSI.VTE where

import Data.List (intersperse)
import Data.Map (Map, toList)
import Data.String (IsString)

-- | A more properly generic implementation
intercalate :: Monoid m => m -> [m] -> m
intercalate x = mconcat . intersperse x

osc :: IsString s => s
osc = "\ESC]"

st :: IsString s => s
st = "\ESC\\"

setLink :: (IsString s, Monoid s) => Map s s -> s -> s
setLink params uri =
  osc <> "8;" <> (intercalate ":" . fmap (\(k, v) -> k <> "=" <> v) $ toList params) <> ";" <> uri <> st

unsetLink :: (IsString s, Semigroup s) => s
unsetLink = osc <> "8;;" <> st
