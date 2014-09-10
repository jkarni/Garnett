
module Text.Garnett.BashWriter where

import Text.PrettyPrint.Free
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map

import Text.Garnett.Definition

{-
bashFmt :: Fmt
bashFmt = Fmt "bash"

lkupBash :: Map.Map Fmt a -> Maybe a
lkupBash m =  Map.lookup bashFmt m
          <|> Map.lookup defaultFmt m

fnName :: Garnett -> Doc Garnett
fnName (Garnett b) = char '_' <> text pName <> text " (){"
    where pName = fromMaybe "prog" $ lkupBash $ _progName b

localDefs :: Doc Garnett
localDefs = text "local cur prev"
    `above` text "COMPREPLY=()"
    `above` text "cur=${COMP_WORDS[COMP_CWORD]}"
    `above` text "prev=${COMP_WORDS[COMP_CWORD - 1]}"

curCase :: Doc Garnett -> Doc Garnett
curCase d = text "case \"$cur\" in"
    `above` indent 4 (text "-*)" <> d)
    `above` text "esac"

ret :: Doc Garnett
ret = text "return 0"

allBash :: Garnett -> Maybe (Doc Garnett)
allBash g@(Garnett b) = do
    short <- getShortCompl b bashFmt <|> getShortCompl b defaultFmt
    long  <- getLongCompl  b bashFmt <|> getLongCompl  b defaultFmt
    let shortDoc = Map.fold (\a b -> b <> text " -" <> char a) empty short
    let longDoc = Map.fold (\a b -> b <> text " --" <> text a) empty long
    let caseLine = text "COMPREPLY=( $( compgen -W '"
                <> shortDoc
                <> longDoc
                <> text "' -- $cur ) );;"
    return $ fnName g `above` indent 4 (localDefs `above` curCase caseLine)
-}
