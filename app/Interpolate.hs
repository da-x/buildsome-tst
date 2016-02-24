{-|

Ripped off interpolateString for performance and reimplementation testing.

|-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad
import Prelude.Compat hiding (FilePath)
import qualified Text.Parsec.Pos as ParsecPos
import Lib.FilePath (FilePath)
import Control.DeepSeq (NFData(..), force, deepseq)
import Control.DeepSeq.Generics (genericRnf)
import Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as BS8
import qualified Lib.FilePath as FilePath
import           Control.Applicative ((<|>))
import GHC.Generics (Generic)

import Text.Read.Lex
import Text.Read

import Lib.TimeIt
import Lib.Parsec () -- instance Binary SourcePos


import Data.Maybe
import qualified Text.Parsec as P

data TargetType output input = Target
  { targetOutputs :: [output]
  , targetInputs :: [input]
  , targetOrderOnlyInputs :: [input]
  , targetCmds :: ByteString
  , targetPos :: ParsecPos.SourcePos
  } deriving (Show, Read, Generic)

instance Read ParsecPos.SourcePos where
   readPrec = do
        arg1 <- readPrec
        Punc "(" <- lexP
        Ident "line" <- lexP
        arg2 <- readPrec
        Punc "," <- lexP
        Ident "column" <- lexP
        arg3 <- readPrec
        Punc ")" <- lexP
        return $ ParsecPos.newPos arg1 arg2 arg3

instance (NFData output, NFData input) => NFData (TargetType output input) where
  rnf = genericRnf

type Target = TargetType FilePath FilePath
type ParserG = P.ParsecT ByteString

#define RELEASE_INLINE(x)   {-# INLINE x #-}

RELEASE_INLINE(escapeSequence)
escapeSequence :: Monad m => ParserG u m ByteString
escapeSequence = build <$> P.char '\\' <*> P.anyChar
  where
    build x y = BS8.singleton x <> BS8.singleton y

interpolateString ::
  Monad m => ParserG u m ByteString -> String -> ParserG u m ByteString -> ParserG u m ByteString
interpolateString escapeParser stopChars dollarHandler =
  concatMany (interpolatedChar ('\'':'"':stopChars) <|> literalString '\'' <|> doubleQuotes)
  where
    concatMany x = BS8.concat <$> P.many x
    RELEASE_INLINE(doubleQuotes)
    doubleQuotes = doubleQuoted <$> P.char '"' <*> concatMany (interpolatedChar "\"\n") <*> P.char '"'
    doubleQuoted begin chars end = BS8.singleton begin <> chars <> BS8.singleton end
    RELEASE_INLINE(interpolatedChar)
    interpolatedChar stopChars' = P.choice
      [ BS8.singleton <$> P.noneOf ('\t' : '\\' : '$' :stopChars')
      , P.char '$' *> dollarHandler
      , escapeParser
      ]
    RELEASE_INLINE(literalString)
    literalString delimiter = do
      x <- P.char delimiter
      str <- BS8.concat <$> P.many p
      y <- P.char delimiter
      return $ BS8.singleton x <> str <> BS8.singleton y
      where
        p = escapeSequence <|> (BS8.singleton <$> P.satisfy (`notElem` ['\t', '\n', delimiter]))

RELEASE_INLINE(metaVarId)
metaVarId ::
  Monad m => [FilePath] -> [FilePath] -> [FilePath] ->
  Maybe FilePath ->
  ParserG u m ((FilePath -> FilePath) -> FilePath)
metaVarId outputPaths inputPaths ooInputPaths mStem =
  P.choice $
  [ firstOutput <$ P.char '@'
  , firstInput  <$ P.char '<'
  , allInputs   <$ P.char '^'
  , allOOInputs <$ P.char '|'
  ] ++
  [ ($ stem) <$ P.char '*'
  | Just stem <- [mStem]
  ]
  where
    getFirst err paths = fromMaybe (error err) $ listToMaybe paths
    firstOutput toString = toString $ getFirst "No first output for @ variable" outputPaths
    firstInput  toString = toString $ getFirst "No first input for < variable"  inputPaths
    allInputs   toString = BS8.unwords $ map toString inputPaths
    allOOInputs toString = BS8.unwords $ map toString ooInputPaths

RELEASE_INLINE(metaVarModifier)
metaVarModifier :: Monad m => ParserG u m (FilePath -> FilePath)
metaVarModifier =
  P.choice
  [ FilePath.takeDirectory <$ P.char 'D'
  , FilePath.takeFileName  <$ P.char 'F'
  ]

RELEASE_INLINE(metaVariable)
metaVariable ::
  Monad m => [FilePath] -> [FilePath] -> [FilePath] ->
  Maybe ByteString -> ParserG u m ByteString
metaVariable outputPaths inputPaths ooInputPaths mStem =
  P.choice
  [ P.char '(' *> (vid <*> metaVarModifier) <* P.char ')'
  , vid <*> pure id
  ]
  where
    vid = metaVarId outputPaths inputPaths ooInputPaths mStem

RELEASE_INLINE(horizSpace)
horizSpace :: Monad m => ParserG u m Char
horizSpace = P.char ' '

RELEASE_INLINE(horizSpaces)
horizSpaces :: Monad m => ParserG u m ()
horizSpaces = P.skipMany horizSpace

RELEASE_INLINE(comment)
comment :: Monad m => ParserG u m ()
comment = void $ P.char '#' *> P.many (P.satisfy (`BS8.notElem` "\n"))

RELEASE_INLINE(skipLineSuffix)
skipLineSuffix :: Monad m => ParserG u m ()
skipLineSuffix = horizSpaces <* P.optional comment <* P.lookAhead (void (P.char '\n') <|> P.eof)

interpolateCmds :: Maybe ByteString -> Target -> Target
interpolateCmds mStem tgt@(Target outputs inputs ooInputs cmds pos) =
      let r = tgt { targetCmds = cmd}
          cmd = either (error . show) id $ interpolateMetavars cmds
       in r
  where
    interpolateMetavars =
      P.runParser
      ( P.setPosition pos
        *> (BS8.intercalate "\n" <$> (cmdInterpolate `P.sepBy` P.char '\n')) <*
        P.eof
      ) () ""
    cmdInterpolate =
      interpolateString escapeSequence "#\n"
      (metaVariable outputs inputs ooInputs mStem)
      <* skipLineSuffix

interpolateCmdsAlex :: Maybe ByteString -> Target -> Target
interpolateCmdsAlex mStem tgt@(Target outputs inputs ooInputs cmds pos) =
      let r = tgt
       in r -- TODO!!!

main :: IO ()
main = do
    content <- B.readFile "/tmp/xx"
    let lst = force $ catMaybes $
            flip map (B8.lines content) $ \i -> (readMaybe $ B8.unpack i :: Maybe ((Maybe ByteString, Target), ByteString))
        parsecParse = map oneParsec lst
        oneParsec ((mStem, tgt), _result) =
            interpolateCmds mStem tgt
        alexParse = map oneAlex lst
        oneAlex ((mStem, tgt), _result) =
            interpolateCmdsAlex mStem tgt
    deepseq lst $ do
        printTimeIt "X" $ deepseq parsecParse $ do
            return ()
        printTimeIt "X" $ deepseq alexParse $ do
            return ()
        return ()
    return ()
