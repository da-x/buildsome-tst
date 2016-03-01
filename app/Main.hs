{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           BMake.Base (Unit, UnitF(..), Statement, StatementF(..), substmts)
import           BMake.User (Error, parseUnit, parseWithAlex, stateBase)
import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Text.Encoding as T
import qualified Data.Yaml.Pretty as YAML
import           Lib.TimeIt (printTimeIt)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

data Dirs = Dirs
    { dirsRoot :: FilePath
    , dirsCurMakefile :: FilePath
    }

handleIncludePath :: Dirs -> FilePath -> IO (DList Statement)
handleIncludePath dirs = fmap unit . parse (dirsRoot dirs)

handleIncludeStr :: Dirs -> FilePath -> IO (DList Statement)
handleIncludeStr dirs ('/':path) = handleIncludePath dirs $ dirsRoot dirs </> path
handleIncludeStr dirs path = handleIncludePath dirs $ dirsCurMakefile dirs </> path

handleInclude :: Dirs -> Statement -> IO (DList Statement)
handleInclude dirs (Include path) =
    case reads pathStr of
    [(quotedPath, "")] -> handleIncludeStr dirs quotedPath
    _ -> handleIncludeStr dirs pathStr
    where
        pathStr = BL8.unpack path
handleInclude dirs other = DList.singleton <$> substmts (handleIncludes dirs) other

handleIncludes :: Dirs -> DList Statement -> IO (DList Statement)
handleIncludes dirs = fmap mconcat . mapM (handleInclude dirs) . DList.toList

parseSingle :: BL8.ByteString -> IO (Either Error Unit)
parseSingle = printTimeIt "parse" . evaluate . force . parseUnit

parse :: FilePath -> FilePath -> IO Unit
parse rootDir makefile = do
    content <- BL.readFile makefile
    let initialParse = parseWithAlex stateBase content
    putStrLn makefile
    let dirs = Dirs rootDir (FilePath.takeDirectory makefile)
    case initialParse of
        Left str -> fail str
        Right tokens -> do
            res <- parseSingle content
            case res of
                Left x -> do
                    forM_ tokens print
                    fail $ show x
                Right ast -> do
                    ast' <- Unit <$> handleIncludes dirs (unit ast)
                    return ast'

main :: IO ()
main = do
    [makefile] <- getArgs
    ast <- printTimeIt "total" $ parse (FilePath.takeDirectory makefile) makefile
    let astf = T.decodeUtf8 . B.concat . BL.toChunks <$> ast
    B.putStr $ YAML.encodePretty YAML.defConfig astf
    return ()
