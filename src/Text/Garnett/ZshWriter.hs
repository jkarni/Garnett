
{- |
    Module : Text.Garnett.BashWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to zsh completion script.
-}
module Text.Garnett.ZshWriter where

import Data.Maybe
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Free

import Text.Garnett.BashWriter
import Text.Garnett.Definition

{-data ZshArg = ZshArg { exclude -}

optionLn :: Option -> Free BashF ()
optionLn opt = do
    let i = opt ^. input
    let addI x = if isJust i then x ++ "+"
    let d = opt ^. optDesc >>= lkupBash
    let addD x = if isJust d then x ++ "[" ++ fromJust d ++ "]"
                             else x
    when (isJust $ opt ^. shortOpt) $ do
        stmt $ addD $ "\" -" ++ opt ^. shortOpt

allZsh :: GarnettFile -> Free BashF ()
allZsh gf = do
    let pName = T.unpack $ gf ^. progName
    stmt $ "#compdef " ++ pName
    stmt $ "typeset -A opt_args"
    stmt "local context state line"
    stmt $ "arguments -s -S \"
