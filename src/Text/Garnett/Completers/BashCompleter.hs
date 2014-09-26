{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{- |
    Module : Text.Garnett.Completers.BashCompleter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to a bash completion script.
-}
module Text.Garnett.Completers.BashCompleter where

import Data.List hiding (group)
import Control.Lens
import Control.Arrow ((&&&))
import Control.Monad.Free
import qualified Data.Map as Map
import qualified Data.Text as T

import Text.Garnett.Definition
import Text.Garnett.Completers.ShellDSL

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
    fromGarnett = toDoc . allBash
--------------------------------------------------------------------------
-- Generate appropriate code
--
--------------------------------------------------------------------------
lkupBash :: Map.Map Fmt a -> Maybe a
lkupBash = lkup (undefined::Bash)

-- | Create a completion function for a given parser.
eachParser :: GParser -> Free ShellF ()
eachParser gp = do
    let shorts = foldr (\a b -> b ++ " -" ++ return a) "" $ getShortCompls gp
    let longs = " --" ++ (intercalate " --" $ map T.unpack $ getLongCompls gp)
    let compgen = shorts ++ longs
    define ('_':(T.unpack $ gp ^. parserName)) $ do
        stmt "local cur prev words"
        stmt "COMPREPLY=()"
        stmt "_get_comp_words_by_ref cur prev words"
        caseStmt "cur" [( "-*", stmt ("COMPREPLY=( $( compgen -W '"
                                     ++ compgen ++ "' -- $cur ))"))
                       ]

-- | A function that checks what subparser we're in, and delegates
-- completion to the corresponding bash function.
delegator :: GarnettFile -> Free ShellF ()
delegator gf = do
    let myName = '_':(T.unpack $ gf ^. progName)
    let pNames = fmap T.unpack $ gf ^. mainParser . subparsers ^.. folded . parserName
    comment $ "Handles completion on subparser names, and delegates "
    comment $ "completion to the subparser completion functions."
    define myName $ do
        stmt "local cur prev words"
        stmt "COMPREPLY=()"
        stmt "_get_comp_words_by_ref cur prev words"
        ifStmt (stmt "${words[@]} -eq 2")
               (stmt $ "COMPREPLY=( $( compgen -W '" ++ unwords pNames ++ "' -- $cur ))")
               (caseStmt "words[1]" $ fmap (id &&& \x -> stmt $ '_':x ++ "()")
                                           pNames)
        stmt "return 0"


-- | The complete bash script.
allBash :: GarnettFile -> Free ShellF ()
allBash gf = do
    comment $ "Completion script for " ++ (T.unpack $ gf ^. progName)
    comment $ "Generated with Garnett"
    mapM_ eachParser (gf ^. mainParser . subparsers)
    delegator gf
    -- stmt "complete -F

