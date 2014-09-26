{-# LANGUAGE TemplateHaskell #-}
{- |
    Module : Text.Garnett.Writers.HaskellWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com>
    Stability : alpha

Convert a GarnettFile to a Haskell option-parsing module
-}

module Text.Garnett.Writers.HaskellWriter where

import Data.Char
import Data.String.Conversions
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Garnett.Definition


writer :: GarnettFile -> String  -- should this be @Text.PrettyPrint.Free.Doc e@?
writer gf = pprint $ [buildType (_mainParser gf), buildFun (_mainParser gf)]


{-

data GarnettOptions = Gar

garnettParse :: Parser GarnettOptions
garnettParse = GarnettOptions

-}


buildType :: GParser -> Dec
buildType gparser = DataD [] dataName [] [RecC dataName fields] []
  where
    dataName = mkName . camelizeCap . cs $ _parserName gparser
    fields = map f $ _options gparser
      where
        f :: Option -> (Name, Strict, Type)
        f option = (mkName . cs $ _optName option, NotStrict, g $ _input option)

        g :: Maybe OptionInputTy -> Type
        g Nothing = ConT ''Bool
        g (Just OITInt) = ConT ''Int
        g (Just OITFloat) = ConT ''Float
        g (Just OITString) = ConT ''String
        g (Just OITFile) = ConT ''FilePath
        g (Just OITDir) = ConT ''FilePath
        g (Just OITBool) = ConT ''Bool
        g (Just (OITList t)) = AppT ListT (g (Just t))


buildFun :: GParser -> Dec
buildFun _ = FunD (mkName "garnettParse") [] -- [mkName "GarnettOptions"]


camelizeCap :: String -> String
camelizeCap "" = ""
camelizeCap (c:cs) = toUpper c : cs
