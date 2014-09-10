module Main where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import Data.Yaml
import Data.Aeson
import Data.Either
import Test.Hspec

import Text.Garnett.Definition
--import Text.Garnett.BashWriter
import Paths_garnett


main :: IO ()
main = hspec $ do
    describe "decode" $ do
        it "successfully decodes the example Garnett file" $ do
            ex <- decodeGFile "example.yaml"
            isRight ex `shouldBe` True


decodeGFile :: FilePath -> IO (Either String GarnettFile)
decodeGFile fp = do
        fp' <- getDataFileName fp
        liftM eitherDecode $ BS.readFile fp'

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

