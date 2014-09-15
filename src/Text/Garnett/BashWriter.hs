{-# LANGUAGE OverloadedStrings #-}
{- |
    Module : Text.Garnett.BashWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to a bash completion script.
-}
module Text.Garnett.BashWriter where

import Text.PrettyPrint.Free
import Control.Applicative
import Data.Maybe
import Data.List
import Control.Lens
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Garnett.Definition

--------------------------------------------------------------------------
-- BashWriter
--
--   In broad outlines, we do the following: For each subparser, create a
--   bash function that can handle completion for that parser. Then create
--   another function that can pick the right subparser based on completion
--   so far, as well as complete parsers. Add a few helper predefined bash
--   functions.
--------------------------------------------------------------------------
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

-- | Take a list of regex-docs pairs, and create a case switch that maps
-- the regex to the doc
--
--  E.g.: curCase [("-*", text "echo 'hi'")]
--        > case "$cur" in
--        >   -*) echo 'hi'
--        > esac
curCase :: [(String, Doc Bash)] -> Doc Bash
curCase rds = text "case \"$cur\" in"
    `above` indent 4 (mconcat $ fmap go rds)
    `above` text "esac"
  where go (r, d) = text r <> char ')' <> d

ret :: Doc Bash
ret = text "return 0"

-- | Bash function for a parser. A main function delegates completion to
-- the appropriate subparser (which might just be 'main', the unnamed
-- subparser).
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

-- | A function that checks what subparser we're in, and delegates
-- completion to the corresponding bash function.
{-delegator :: GarnettFile -> Doc Bash-}
{-delegator gf = myName-}
  {-where myName = char '_' <> text (T.unpack $ gf ^. progName) <> text "(){"-}
        {-pNames = gf ^. gParsers ^.. folded . parserName-}
        {-cases = myName `above` indent 4 (localDefs-}
                                         {-`above` -}

-- | The complete bash script.
allBash :: GarnettFile -> Doc Bash
allBash gf = pFns
    where pFns = foldr above empty $ catMaybes $ fmap bashFor ( gf ^. gParsers)

