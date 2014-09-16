{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Data.List hiding (group)
import Control.Lens
import Control.Arrow ((&&&))
import Control.Monad.Free
import Control.Monad
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
    fromGarnett = toDoc . allBash
--------------------------------------------------------------------------
-- Bash DSL
--
--   Only the subset of bash that we care about is represented.
--------------------------------------------------------------------------

data BashF next =
      Define String (Free BashF ()) next
    | CaseStmt String [(String, Free BashF ())] next
    | If (Free BashF ()) (Free BashF ()) (Free BashF ()) next
    | For String String (Free BashF ()) next
    | Comment String next
    | SimpleStmt String next
    | Done
    deriving (Functor, Show)


define :: String -> Free BashF () -> Free BashF ()
define x y = liftF (Define x y ())

caseStmt :: String -> [(String, Free BashF ())] -> Free BashF ()
caseStmt var cs = liftF (CaseStmt var cs ())

ifStmt :: Free BashF () -> Free BashF () -> Free BashF () -> Free BashF ()
ifStmt cond f s = liftF (If cond f s ())

forStmt :: String -> String -> Free BashF () -> Free BashF ()
forStmt var range stmts = liftF (For var range stmts ())

-- Catch-all statement (any other case can just be encoded as
-- `stmt <string>`)
stmt :: String -> Free BashF ()
stmt x = liftF (SimpleStmt x ())

comment :: String -> Free BashF ()
comment x = liftF (Comment x ())


--------------------------------------------------------------------------
-- Pretty printing
--
--------------------------------------------------------------------------


toDoc :: (Show r) => Free BashF r -> Doc Bash
toDoc (Free (Define name dfn cnt)) = text name <> "(){"
                             `above` indent 4 (toDoc dfn)
                             `above` "}"
                             `above` toDoc cnt
toDoc (Free (CaseStmt var cs cnt)) = "case \"$" <> text var <> "\" in"
                             `above` indent 4 (vsep $ fmap unr cs)
                             `above` "esac"
                             `above` toDoc cnt
                           where unr (s, d) = text s <> char ')'
                                          <+> group (toDoc d <+> ";;")
toDoc (Free (If cond f s cnt)) = "if [" <+> group (toDoc cond) <+> "]; then"
                         `above` indent 4 (toDoc f)
                         `above` "else"
                         `above` indent 4 (toDoc s)
                         `above` "fi"
                         `above` toDoc cnt
toDoc (Free (For var range stmts cnt)) = "for" <+> text var <+> "in" <+> text range
                                   `above` "do"
                                   `above` indent 4 (toDoc stmts)
                                   `above` "done"
                                   `above` toDoc cnt
toDoc (Free (Comment x cnt)) = "#" <+> text x `above` toDoc cnt
toDoc (Free (SimpleStmt x cnt)) = text x `above` toDoc cnt
toDoc (Free Done) = empty
toDoc (Pure _) = empty

--------------------------------------------------------------------------
-- Generate appropriate code
--
--------------------------------------------------------------------------
lkupBash :: Map.Map Fmt a -> Maybe a
lkupBash = lkup (undefined::Bash)

-- | Create a completion function for a given parser.
eachParser :: GParser -> Free BashF ()
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
delegator :: GarnettFile -> Free BashF ()
delegator gf = do
    let myName = '_':(T.unpack $ gf ^. progName)
    let pNames = fmap T.unpack $ gf ^. gParsers ^.. folded . parserName
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
allBash :: GarnettFile -> Free BashF ()
allBash gf = do
    comment $ "Completion script for " ++ (T.unpack $ gf ^. progName)
    comment $ "Generated with Garnett"
    mapM_ eachParser (gf ^. gParsers)
    delegator gf
    stmt "complete -F

