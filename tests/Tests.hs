module Main where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import Data.Yaml
import Data.Aeson
import Data.Maybe
import Data.Either
import Test.Hspec

import Text.Garnett.Definition
import Text.Garnett.BashWriter
import Paths_garnett


main :: IO ()
main = hspec $ do
    describe "decode" $ do
        it "successfully decodes the example Garnett file" $ do
            ex <- decodeGFile "example.yaml"
            isRight ex `shouldBe` True

printBash :: FilePath -> IO ()
printBash fp = do
       f <- decodeGFile fp
       case f of
           Left exc -> print exc
           Right gf -> print $ toDoc $ allBash gf

decodeGFile :: FilePath -> IO (Either ParseException GarnettFile)
decodeGFile fp = do
        fp' <- getDataFileName fp
        decodeFileEither fp'

{-
defaultBlock = Block { _progName    = Map.fromList [(defaultFmt, "test")]
                     , _authorName  = Map.fromList [(defaultFmt, "test name")]
                     , _authorEmail = Map.fromList [(defaultFmt, "test@email")]
                     , _shortDesc   = Map.fromList [(defaultFmt, Markup "A test prog")]
                     , _completions = Map.fromList [(defaultFmt, comp)]
                     }
    where comp = Completions { _shortCompl = Map.fromList [("help", 'h')]
                             , _longCompl  = Map.fromList [("help", "help")]
                             }
-}

