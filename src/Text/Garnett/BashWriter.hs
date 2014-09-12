
{-# LANGUAGE OverloadedStrings #-}
module Text.Garnett.BashWriter where

import Text.PrettyPrint.Free
import Control.Applicative
import Data.Maybe
import Data.List
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Garnett.Definition

data Bash

instance GarnettWriter Bash where
    fmt _ = Fmt "bash"
    fromGarnett = allBash

lkupBash :: Map.Map Fmt a -> Maybe a
lkupBash = lkup (undefined::Bash)


fnName :: GParser -> Doc Bash
fnName b = char '_' <> text (T.unpack $ b ^. parserName) <> text " (){"

localDefs :: Doc Bash
localDefs = text "local cur prev words"
    `above` text "COMPREPLY=()"
    `above` text "_get_comp_words_by_ref cur prev words"

curCase :: Doc Bash -> Doc Bash
curCase d = text "case \"$cur\" in"
    `above` indent 4 (text "-*)" <> d)
    `above` text "esac"

ret :: Doc Bash
ret = text "return 0"

bashFor :: GParser -> Maybe (Doc Bash)
bashFor g = do
    let shorts = foldr (\a b -> b ++ " -" ++ return a) "" $ getShortCompls g
    let longs = " --" ++ (intercalate " --" $ map T.unpack $ getLongCompls g)
    let caseLine = text "COMPREPLY=( $( compgen -W '"
                <> text shorts
                <> text longs
                <> text "' -- $cur ) );;"
    return $ fnName g `above` indent 4 (localDefs
                                       `above` curCase caseLine
                                       `above` ret)
                      `above` text "}"

allBash :: GarnettFile -> Doc Bash
allBash gf = pFns
    where pFns = foldr above empty $ catMaybes $ fmap bashFor ( gf ^. gParsers)

