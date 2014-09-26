{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{- |
    Module : Text.Garnett.Definition
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com
    Stability : alpha

The internal data types definitions.
-}
module Text.Garnett.Definition where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.At
import           Control.Lens.TH
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict   as H
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid
import qualified Data.Text             as T
import           Data.Yaml
import           Data.Vector
import           GHC.Generics
import           GHC.TypeLits
import           Text.PrettyPrint.Free

--------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------

data Garnett a b = Garnett


-- | The output format - e.g. 'man', 'bash', 'default'. 'default' is used
-- when the more specific format is not specified.
newtype Fmt = Fmt { getFmt :: T.Text } deriving (Eq, Ord, Generic, Show)
type FmtMap = Map.Map Fmt

-- | The type of the input an option accepts
data OptionInputTy = OITInt
                   | OITFloat
                   | OITString
                   | OITFile
                   | OITDir
                   | OITBool
                   | OITList OptionInputTy
                   deriving (Show, Eq)


-- | An option.
-- TODO: Add default values.
-- TODO: Currently the number of inputs is fixed. Accept
--       a possibly-variable (and possibly infinite) number of inputs
data Option =
    Option { _shortOpt    :: Maybe Char
           , _longOpt     :: Maybe T.Text
           , _input       :: Maybe OptionInputTy
           , _optName     :: T.Text  -- ^ field name of option in parsed result
           , _optDesc     :: Maybe (FmtMap T.Text)
           , _optMetavar  :: Maybe T.Text
           , _optRequired :: Bool   -- ^ default 'False'
           } deriving (Show)
$(makeLenses ''Option)

data GParser = GParser { _parserName :: T.Text
                       , _options    :: [Option]
                       , _intro      :: Maybe (FmtMap T.Text)
                       , _seeAlso    :: Maybe T.Text
                       , _subparsers :: [GParser]
                       } deriving (Show)

$(makeLenses ''GParser)
data GarnettFile = GarnettFile { _progName    :: T.Text
                               , _authorName  :: T.Text
                               , _authorEmail :: Maybe T.Text
                               , _mainParser  :: GParser
                               } deriving (Generic, Show)
$(makeLenses ''GarnettFile)
data Markup = Markup T.Text deriving (Generic)



-- | Class for things that can create themselves from a Garnett.
class GarnettWriter a where
    fmt :: a -> Fmt
    fromGarnett :: GarnettFile -> Doc a


--------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------

instance FromJSON Fmt
instance FromJSON a => FromJSON (FmtMap a) where
     parseJSON obj = parseJSON obj >>= return . (Map.mapKeys Fmt)

instance FromJSON OptionInputTy where
    parseJSON (String "int")       = pure OITInt
    parseJSON (String "float")     = pure OITFloat
    parseJSON (String "string")    = pure OITString
    parseJSON (String "str")       = pure OITString
    parseJSON (String "file")      = pure OITFile
    parseJSON (String "dir")       = pure OITDir
    parseJSON (String "directory") = pure OITDir
    parseJSON (Array (toList -> [x])) = OITList <$> parseJSON x
    parseJSON bad = error $ show bad

instance FromJSON Option where
    parseJSON (Object v) = Option <$> v .:? "short"
                                  <*> v .:? "long"
                                  <*> v .:? "input"
                                  <*> v .:  "name"
                                  <*> v .:? "description"
                                  <*> v .:? "metavar"
                                  <*> v .:? "required" .!= False
    parseJSON _ = mzero

instance FromJSON GParser where
    parseJSON (Object v) = GParser <$> v .: "name"
                                   <*> v .: "options"
                                   <*> v .:? "intro"
                                   <*> v .:? "see-also"
                                   <*> v .:?  "subparsers" .!= []
    parseJSON _ = mzero

instance FromJSON GarnettFile where
    parseJSON (Object v) = GarnettFile <$> v .: "prog-name"
                                       <*> v .: "author"
                                       <*> v .:? "email"
                                       <*> ( GParser <$> return "main"
                                                     <*> v .: "options"
                                                     <*> v .:  "intro"
                                                     <*> v .:? "see-also"
                                                     <*> v .:? "subparsers" .!= []
                                           )
    parseJSON _ = mzero

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------
defaultFmt :: Fmt
defaultFmt = Fmt "default"

lkup :: (GarnettWriter w) => w -> FmtMap a -> Maybe a
lkup w = Map.lookup (fmt w)

getShortCompls :: GParser -> [Char]
getShortCompls p = catMaybes $ (p ^. options) ^.. folded . shortOpt

getLongCompls :: GParser -> [T.Text]
getLongCompls p = catMaybes $ (p ^. options) ^.. folded . longOpt
