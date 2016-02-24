{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BMake.User            (parseUnit, parseWithAlex, stateBase)
import           Control.DeepSeq       (NFData (..), deepseq, force)
import           Control.Monad         (forM, forM_, when)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text.Encoding    as T
import qualified Data.Yaml.Pretty      as YAML
import           Lib.TimeIt
import           System.Directory      (doesDirectoryExist,
                                        getDirectoryContents)
import           System.Environment    (getArgs)
import           System.FilePath       ((</>))

getRecursiveContents :: FilePath -> IO [ByteString]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [B8.pack path]
    return (concat paths)

main :: IO ()
main = do
    [rootDir] <- getArgs

    nodes <- getRecursiveContents rootDir
    forM_ nodes $ \file -> do
        when (".mk" `B.isSuffixOf` file) $ do
            b <- BL.readFile $ B8.unpack file
            putStrLn ""
            B8.putStrLn file
            let initialParse = parseWithAlex stateBase b
            case initialParse of
                Left str -> print str
                Right tokens -> do
                    forM_ tokens print
                    case parseUnit b of
                        Left x -> print x
                        Right ast -> do
                            print ast
                            let astf = fmap (T.decodeUtf8 . B.concat . BL.toChunks) ast
                            B.putStr $ YAML.encodePretty YAML.defConfig astf
                            return ()
        return ()

    -- Do it again for timing

    printTimeIt "Total AST parse time" $
        forM_ nodes $ \file -> do
            when (".mk" `B.isSuffixOf` file) $ do
                printTimeIt ("-- Parse time of " ++ show file) $ do
                    b <- BL.readFile $ B8.unpack file
                    let parsed =
                            case parseUnit b of
                                Left str -> error $ show str
                                Right ast -> ast
                    deepseq (force parsed) $
                        return ()
