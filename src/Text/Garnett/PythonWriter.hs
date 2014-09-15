{- |
    Module : Text.Garnett.PythonWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to a Python option-parsing module
-}

module Text.Garnett.HaskellWriter where

import Text.PrettyPrint.Free
import qualified Data.Text as T
import Control.Lens

data Python

ttod :: T.Text -> Doc Python
ttod = text . T.unpack

imports :: Doc Python
imports = text "import argparse"

mkParser :: GParser -> Doc Python
mkParser gp = text "_parser_for_" <> text (gp ^. parserName)
            <+> char '='
            <+> text "argparse.ArgumentParser()"

-- | Add arguments to the parser `pName`
{-addArgs :: String -> Option -> Doc Python-}
{-addArgs pName opt = text pName <> ".addArgument(" <> ttod (opt ^. shortOpt)-}
                                                  {-<> ttod (opt ^. longOpt)-}
