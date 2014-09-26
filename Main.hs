{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude
import Data.Yaml
import Data.Aeson
import Data.String.Conversions
import Safe

import Text.Garnett.Definition
import Text.Garnett.Writers.HaskellWriter (writer)
--import Text.Garnett.BashWriter

import Data.ByteString.Lazy as LBS hiding (writeFile, putStr, putStrLn)

main :: IO ()
main =
    decodeFileEither "example.yaml" >>=
    writer . either (error . show) id >>=
    putStrLn . (<> "\n---\n") . ("\n---\n" <>)
