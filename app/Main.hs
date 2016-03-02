{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BMake.User            (parseUnit, parseWithAlex, stateBase)
import           Control.DeepSeq       (force)
import           Control.Exception     (evaluate)
import           Control.Monad         (forM_)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text.Encoding    as T
import qualified Data.Yaml.Pretty      as YAML
import           Lib.TimeIt            (printTimeIt)
import           System.Environment    (getArgs)

main :: IO ()
main = do
    [makefile] <- getArgs
    content <- BL.readFile makefile
    let initialParse = parseWithAlex stateBase content
    putStrLn makefile
    case initialParse of
        Left str -> print str
        Right tokens -> do
            res <- (printTimeIt "parse" . evaluate . force . parseUnit) content
            case res of
                Left x -> do
                    putStrLn "ERROR"
                    forM_ tokens print
                    print x
                Right ast -> do
                    print ast
                    let astf = T.decodeUtf8 . B.concat . BL.toChunks <$> ast
                    B.putStr $ YAML.encodePretty YAML.defConfig astf
                    return ()
