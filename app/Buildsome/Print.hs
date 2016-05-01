{-# LANGUAGE OverloadedStrings          #-}

module Buildsome.Print
  ( posText ) where

import           Data.String (IsString(..))
import           Text.Parsec (SourcePos)

posText :: (Monoid s, IsString s) => SourcePos -> s
posText _ = ""
