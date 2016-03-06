{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           BMake.Base (Makefile, MakefileF(..), Statement, StatementF(..), substmts)
import           BMake.Interpreter (interpret)
import           BMake.User (Error, parseMakefile)
import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Yaml.Pretty as YAML
import           Lib.TimeIt (printTimeIt)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

type Cache k v = IORef (Map k v)

type ParseCache = Cache FilePath Makefile

data Dirs = Dirs
    { dirsRoot :: FilePath
    , dirsCurMakefile :: FilePath
    }

handleIncludePath :: ParseCache -> Dirs -> FilePath -> IO [Statement]
handleIncludePath cache dirs = fmap unit . parse cache (dirsRoot dirs)

handleIncludeStr :: ParseCache -> Dirs -> FilePath -> IO [Statement]
handleIncludeStr cache dirs ('/':path) = handleIncludePath cache dirs $ dirsRoot dirs </> path
handleIncludeStr cache dirs path = handleIncludePath cache dirs $ dirsCurMakefile dirs </> path

handleInclude :: ParseCache -> Dirs -> Statement -> IO [Statement]
handleInclude cache dirs (Include path) =
    case reads pathStr of
    [(quotedPath, "")] -> handleIncludeStr cache dirs quotedPath
    _ -> handleIncludeStr cache dirs pathStr
    where
        pathStr = BL8.unpack path
handleInclude cache dirs other =
    (: []) <$> substmts (handleIncludes cache dirs) other

handleIncludes :: ParseCache -> Dirs -> [Statement] -> IO [Statement]
handleIncludes cache dirs = fmap concat . mapM (handleInclude cache dirs)

parseSingle :: BL8.ByteString -> IO (Either Error Makefile)
parseSingle = -- printTimeIt "parse" .
              evaluate . force . parseMakefile

memoIO :: Ord k => Cache k v -> (k -> IO v) -> k -> IO v
memoIO cache action k =
    do
        m <- readIORef cache
        case Map.lookup k m of
            Just v -> return v
            Nothing ->
                do
                    v <- action k
                    modifyIORef cache $ Map.insert k v
                    return v

parse :: ParseCache -> FilePath -> FilePath -> IO Makefile
parse cache rootDir =
    memoIO cache $ \makefile -> do
        content <- BL.readFile makefile
        -- putStrLn makefile
        let dirs = Dirs rootDir (FilePath.takeDirectory makefile)
        res <- parseSingle content
        case res of
            Left x -> fail $ show x
            Right ast -> do
                ast' <- Makefile <$> handleIncludes cache dirs (unit ast)
                return ast'

main :: IO ()
main = do
    cache <- newIORef Map.empty
    [makefile] <- getArgs
    ast <-
        printTimeIt "total" $
        parse cache (FilePath.takeDirectory makefile) makefile
--    let astf = T.decodeUtf8 . B.concat . BL.toChunks <$> ast
--    B.putStr $ YAML.encodePretty YAML.defConfig astf
    printTimeIt "total" $ interpret ast
    return ()
