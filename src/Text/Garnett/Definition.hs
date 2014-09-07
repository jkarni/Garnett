{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Garnett.Definition where

import GHC.Generics
import Data.Monoid
import Data.Yaml
import Control.Lens.TH
import Control.Lens
import Control.Lens.At
import qualified Data.Map as Map



newtype Fmt = Fmt { getFmt :: String } deriving (Eq, Ord, Generic)

defaultFmt :: Fmt
defaultFmt = Fmt "default"

data Markup = Markup String deriving (Generic)

data Completions = Completions { _shortCompl :: Map.Map String Char
                               , _longCompl  :: Map.Map String String
                               }

instance Monoid Completions where
    mempty = Completions { _shortCompl = Map.empty
                         , _longCompl  = Map.empty
                         }
    a `mappend` b = undefined -- (Map.union over records)

$(makeLenses ''Completions)

data Block = Block { _progName    :: Map.Map Fmt String
                   , _authorName  :: Map.Map Fmt String
                   , _authorEmail :: Map.Map Fmt String
                   , _shortDesc   :: Map.Map Fmt Markup
                   , _completions :: Map.Map Fmt Completions
                   } deriving Generic
$(makeLenses ''Block)

getComplFor :: Block -> Fmt -> Maybe Completions
getComplFor b f = b ^. completions . (at f)

getShortCompl :: Block -> Fmt -> Maybe (Map.Map String Char)
getShortCompl b f = getComplFor b f <&> view shortCompl

getLongCompl :: Block -> Fmt -> Maybe (Map.Map String String)
getLongCompl b f = getComplFor b f <&> view longCompl

instance Monoid Block where
    mempty = Block { _progName    = Map.empty
                   , _authorName  = Map.empty
                   , _authorEmail = Map.empty
                   , _shortDesc   = Map.empty
                   , _completions = mempty
                   }
    a `mappend` b = undefined -- (Map.union over records)

data Garnett = Garnett Block

instance Monoid Garnett where
    mempty = Garnett mempty
    Garnett a `mappend` Garnett b = Garnett (a <> b)

{-
class GarnettReader a where
    toGarnett :: a -> Garnett
class GarnettWriter a where
    fromGarnett :: Garnett -> a
-}
