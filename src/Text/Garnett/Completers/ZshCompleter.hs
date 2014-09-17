
{- |
    Module : Text.Garnett.BashWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to zsh completion script.
-}
module Text.Garnett.Completers.ZshCompleter where

import Data.List (intersperse)
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Free

import Text.Garnett.Completers.ShellDSL
import Text.Garnett.Definition

data ZshArg = ZshArg { exclude :: [String]
                     , optVar  :: String
                     , optHelp :: String
                     , nArgs   :: Int
                     , hint    :: String
                     , action  :: String
                     }

-- | Turn a ZshArg into a zsh _arguments() syntax.
-- TODO: This should be cleaner
toArgs :: ZshArg -> String
toArgs zArg = ('(':exc ++ ")") ++ addDash (optVar zArg) ++ n
            ++ ('[':(optHelp zArg) ++ "]") ++ (':':(hint zArg)++ ":")
            ++ action zArg
    where addDash a@(_:[]) = '-':a
          addDash xs       = "--" ++ xs
          exc = concat $ intersperse " " (fmap addDash $ exclude zArg)
          n = if nArgs zArg > 0 then "+" else ""


{-optionLn :: Option -> Free ShellF ()-}
{-optionLn opt = do-}
    {-let i = opt ^. input-}
    {-let addI x = if isJust i then x ++ "+" else ""-}
    {-let d = fmap T.unpack $ opt ^. optDesc >>= lkupBash-}
    {-let addD x = if isJust d then x ++ "[" ++ fromJust d ++ "]"-}
                             {-else x-}
    {-when (isJust $ opt ^. shortOpt) $ do-}
        {-stmt $ addD $ insert (fromMaybe ' ' (opt ^. shortOpt)) "\" -"-}

allZsh :: GarnettFile -> Free ShellF ()
allZsh gf = do
    let pName = T.unpack $ gf ^. progName
    stmt $ "#compdef " ++ pName
    stmt $ "typeset -A opt_args"
    stmt "local context state line"
    stmt $ "arguments -s -S \""
