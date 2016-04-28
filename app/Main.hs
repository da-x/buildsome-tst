{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------------------
import           Control.DeepSeq            (force, deepseq)
import           Control.Exception          (evaluate)
import           Control.Monad              (forM_)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Yaml.Pretty as YAML
import           System.Environment         (getArgs)
import           System.FilePath            ((</>))
import qualified System.FilePath            as FilePath
----
import           BMake.Base                 (Makefile, MakefileF (..),
                                             Statement, StatementF (..),
                                             substmts)
import           BMake.Interpreter          (interpret)
import           BMake.User                 (Error (..), parseMakefile,
                                             parseWithAlex, stateBase)
import qualified Lib.Makefile.Types as MT
import qualified Lib.Makefile.Parser        as OLD
import           Lib.TimeIt                 (printTimeIt)
------------------------------------------------------------------------------------------

type Cache k v = IORef (Map k v)

type ParseCache = Cache FilePath Makefile

data Dirs = Dirs
    { dirsRoot        :: FilePath
    , dirsCurMakefile :: FilePath
    }

handleIncludePath :: ParseCache -> Dirs -> FilePath -> IO [Statement]
handleIncludePath cache dirs = fmap unit . newParse cache (dirsRoot dirs)

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

newParse :: ParseCache -> FilePath -> FilePath -> IO Makefile
newParse cache rootDir =
    memoIO cache $ \makefile -> do
        content <- BL.readFile makefile
        -- putStrLn makefile
        let dirs = Dirs rootDir (FilePath.takeDirectory makefile)
        res <- parseSingle content
        case res of
            Left (Error line col str) -> do
                case parseWithAlex stateBase content of
                    Right tokens -> do
                        forM_ tokens print
                    Left _ -> return ()
                fail $ makefile ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ str
            Right ast -> do
                ast' <- Makefile <$> handleIncludes cache dirs (unit ast)
                return ast'

main :: IO ()
main = do
    cache <- newIORef Map.empty

    let reportResult makefile = do
            putStrLn $ "deepseqing"
            printTimeIt "deepseq" $
                putStrLn $ deepseq makefile "done"
            print $ length $ MT.makefileTargets makefile
            print $ length $ MT.makefilePatterns makefile
            print $ length $ MT.makefilePhonies makefile
            return ()

    let newCode makefilePath = do
            putStrLn "New Makefile parser:"

            ast <-
                printTimeIt "total" $
                newParse cache (FilePath.takeDirectory makefilePath) makefilePath
                -- let astf = T.decodeUtf8 . B.concat . BL.toChunks <$> ast
                -- B.putStr $ YAML.encodePretty YAML.defConfig astf
            makefile <- printTimeIt "total" $ interpret ast Map.empty
            reportResult makefile
            return ()

    let oldCode makefilePath = do
            putStrLn "Running old Makefile parser:"

            makefile <- printTimeIt "total" $ do
                OLD.parse (B8.pack makefilePath) Map.empty
            reportResult makefile

    getArgs >>= \case
        [] -> putStrLn "No parameters given"
        ["new", makefilePath] -> do
            newCode makefilePath

        ["old", makefilePath] -> do
            oldCode makefilePath

        [makefilePath] -> do
            oldCode makefilePath
            newCode makefilePath
        _  -> putStrLn "Invalid command line"
