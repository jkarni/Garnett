{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{- |
    Module : Text.Garnett.Completer.ShellDSL
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

A small shell DSL
-}

module Text.Garnett.Completers.ShellDSL where

import Text.PrettyPrint.Free
import Control.Monad.Free
--------------------------------------------------------------------------
-- Shell DSL
--
--   Only the subset of shell that we care about is represented.
--------------------------------------------------------------------------

data ShellF next =
      Define String (Free ShellF ()) next
    | CaseStmt String [(String, Free ShellF ())] next
    | If (Free ShellF ()) (Free ShellF ()) (Free ShellF ()) next
    | For String String (Free ShellF ()) next
    | Comment String next
    | SimpleStmt String next
    | Done
    deriving (Functor, Show)


define :: String -> Free ShellF () -> Free ShellF ()
define x y = liftF (Define x y ())

caseStmt :: String -> [(String, Free ShellF ())] -> Free ShellF ()
caseStmt var cs = liftF (CaseStmt var cs ())

ifStmt :: Free ShellF () -> Free ShellF () -> Free ShellF () -> Free ShellF ()
ifStmt cond f s = liftF (If cond f s ())

forStmt :: String -> String -> Free ShellF () -> Free ShellF ()
forStmt var range stmts = liftF (For var range stmts ())

-- Catch-all statement (any other case can just be encoded as
-- `stmt <string>`)
stmt :: String -> Free ShellF ()
stmt x = liftF (SimpleStmt x ())

comment :: String -> Free ShellF ()
comment x = liftF (Comment x ())


--------------------------------------------------------------------------
-- Pretty printing
--
--------------------------------------------------------------------------


toDoc :: (Show r) => Free ShellF r -> Doc a
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

