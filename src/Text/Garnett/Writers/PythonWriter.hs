{- |
    Module : Text.Garnett.Writers.PythonWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

Convert a GarnettFile to a Python option-parsing module
-}

module Text.Garnett.Writers.PythonWriter where

import Text.PrettyPrint.Free
import Control.Monad.Free
import Control.Lens

import qualified Data.Text as T

import Text.Garnett.Definition

