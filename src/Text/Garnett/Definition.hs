{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Garnett.Definition where

import GHC.Generics
import Data.Monoid
import Data.Yaml
import Data.Aeson
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Lens.TH
import Control.Lens
import Control.Lens.At
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as H
import Text.PrettyPrint.Free

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
                   | OITList OptionInputTy
                   deriving (Show, Eq)

-- | An option.
data Option =
    Option { _shortOpt :: Maybe Char
           , _longOpt  :: Maybe T.Text
           , _input    :: Maybe OptionInputTy
           , _optName  :: T.Text  -- ^ field name of option in parsed result
           , _optDesc  :: Maybe T.Text
           } deriving (Show)
$(makeLenses ''Option)

data GParser = GParser { _parserName :: T.Text
                       , _options    :: [Option]
                       , _intro      :: Maybe (FmtMap T.Text)
                       , _seeAlso    :: Maybe T.Text
                       } deriving (Show)

$(makeLenses ''GParser)
data GarnettFile = GarnettFile { _progName    :: T.Text
                               , _authorName  :: T.Text
                               , _authorEmail :: Maybe T.Text
                               , _gParsers    :: [GParser]
                               } deriving (Generic, Show)
$(makeLenses ''GarnettFile)
data Markup = Markup T.Text deriving (Generic)



-- | Class for things that can create themselves from a Garnett.
class GarnettWriter a where
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
    parseJSON (String x)
          | T.head x == '[' && T.last x == ']' = liftM OITList next
          | otherwise                          = mzero
      where next = parseJSON . String . T.init $ T.tail x
    parseJSON _ = mzero


instance FromJSON Option where
    parseJSON (Object v) = Option <$> v .: "short"
                                  <*> v .: "long"
                                  <*> v .: "input"
                                  <*> v .: "name"
                                  <*> v .: "description"
    parseJSON _ = mzero

instance FromJSON GParser where
    parseJSON (Object v) = GParser <$> v .: "name"
                                   <*> v .: "options"
                                   <*> v .: "intro"
                                   <*> v .: "see-also"
    parseJSON _ = mzero

instance FromJSON GarnettFile where
    parseJSON (Object v) = GarnettFile <$> v .: "prog-name"
                                 <*> v .: "author"
                                 <*> v .: "email"
                                 <*> v .: "parsers"
    parseJSON _ = mzero

--------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------
defaultFmt :: Fmt
defaultFmt = Fmt "default"

