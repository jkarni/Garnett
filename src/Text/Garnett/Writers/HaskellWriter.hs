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

import Data.List
import Control.Monad
import Data.Char
import Control.Applicative
import Data.String.Conversions
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Garnett.Definition


writer :: GarnettFile -> IO String  -- should this be @Text.PrettyPrint.Free.Doc e@?
writer gf = runQ $ (intercalate "\n" <$>) . sequence $ pprint <$$> [buildType (_mainParser gf), buildFun (_mainParser gf)]

(<$$>) = fmap . fmap


{-

data GarnettOptions = Gar

garnettParse :: Parser GarnettOptions
garnettParse = GarnettOptions

-}


buildType :: GParser -> Q Dec
buildType gparser = return $ DataD [] dataName [] [RecC dataName fields] []
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


buildFun :: GParser -> Q Dec
buildFun gparser = do
    let dataName = mkName . camelizeCap . cs $ _parserName gparser
        funName = mkName . ("parseOpt" <>) . camelizeCap . cs $ _parserName gparser

        fields = map f $ _options gparser
          where
            f :: Option -> Exp
            f option = error "wef"  --- (mkName . cs $ _optName option, NotStrict, g $ _input option)

    body <- [| VarE (mkName "should_be_dataName") |]

    return $ FunD funName [Clause [] (NormalB body) []]




{-

    body :: Q Exp
    body = error "wef"  --   -- ) (VarE $ mkName "<$>")


data Main
    = Main {verbosity :: GHC.Types.Bool,
            help :: GHC.Types.Bool,
            inputFile :: ([GHC.IO.FilePath]),
            logLevel :: GHC.Types.Bool}

parseOptMain = Main <$>


-}




camelizeCap :: String -> String
camelizeCap "" = ""
camelizeCap (c:cs) = toUpper c : cs

camelizeSmall :: String -> String
camelizeSmall "" = ""
camelizeSmall (c:cs) = toLower c : cs
