{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------------------
import           Control.DeepSeq       (deepseq)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as Map
import qualified Data.Set              as S
import           System.Environment    (getArgs)
----
import qualified Buildsome.BuildMaps   as BuildMaps
import qualified BMake.User            as BMake
import qualified Lib.Makefile.Parser   as OLD
import qualified Lib.Makefile.Types    as MT
import           Lib.TimeIt            (printTimeIt)
------------------------------------------------------------------------------------------

main :: IO ()
main = do
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
            makefile <- printTimeIt "total" $ do
                BMake.parse (B8.pack makefilePath) Map.empty

            -- ToDo: use newer code
            let buildMaps = BuildMaps.make makefile
            let phoniesSet = S.fromList . map snd $ MT.makefilePhonies makefile
            printTimeIt "buildmaps" $
                putStrLn $ deepseq (buildMaps, phoniesSet) "done"

            reportResult makefile
            return ()

    let oldCode makefilePath = do
            putStrLn "Running old Makefile parser:"

            makefile <- printTimeIt "total" $ do
                OLD.parse (B8.pack makefilePath) Map.empty

            let buildMaps = BuildMaps.make makefile
            let phoniesSet = S.fromList . map snd $ MT.makefilePhonies makefile
            printTimeIt "buildmaps" $
                putStrLn $ deepseq (buildMaps, phoniesSet) "done"

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
