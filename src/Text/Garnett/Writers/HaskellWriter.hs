{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
    Module : Text.Garnett.Writers.HaskellWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com>
    Stability : alpha

Convert a GarnettFile to a Haskell option-parsing module
-}

module Text.Garnett.Writers.HaskellWriter where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Yaml hiding (Parser)
import           Data.String.Conversions
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Options.Applicative
import           Text.Garnett.Definition



-- | Given a YAML file with the appropriate data, generates the data types
-- and parsing functions.
--
-- Use it like so:
--   [d| mkOptParser "example.yaml" |]
---
mkOptParser :: FilePath -> Q [Dec]
mkOptParser fp = do
  f <- runIO $ decodeFileEither fp 
  case f of 
    Left err -> error $ show err
    Right gf -> splice gf
                      


splice :: GarnettFile -> Q [Dec]
splice gf = sequence [buildType (_mainParser gf), buildFun (_mainParser gf)]

mkDataName :: GParser -> Name                
mkDataName gp = mkName . (++ "Ty") . titleCase . cs $ _parserName gp

mkFunName :: GParser -> Name                
mkFunName gp = mkName . ("parseOpt" ++) . cs $ _parserName gp

-- | Builds a data type declaration, with a single constructor and multiple
-- records, for a parser.
buildType :: GParser -> Q Dec
buildType gparser = return $ DataD [] dataName [] [RecC dataName fields] []
  where
    dataName = mkDataName gparser 
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
    let 
        dataName = mkDataName gparser 
        funName  = mkFunName gparser 
        -- typeAnnot = SigE <$> varE funName <*> (AppT <$> (conT $ mkName "Parser") <*> (conT dataName))

        fields :: [Q Exp]
        fields | length v /= 0 = v
          where
            v = map f $ _options gparser

            f :: Option -> Q Exp
            f opt = [| option auto $(foldApQExps (varE $ mkName "<>") _all) |]
                where
                    _all :: [Q Exp]
                    _all = catMaybes
                        [ _long opt
                        , _short opt
                        , _help opt
                        ]
                    mkStr :: String -> Q Exp
                    mkStr = return . LitE . StringL

                    mkChar :: Char -> Q Exp
                    mkChar = return . LitE . CharL

                    _long :: Option -> Maybe (Q Exp)
                    _long = fmap (\x -> [| long $(mkStr $ cs x) |]) . _longOpt

                    _short :: Option -> Maybe (Q Exp)
                    _short = fmap (\x -> [| short $(mkChar x) |]) . _shortOpt

                    _help :: Option -> Maybe (Q Exp)
                    _help = fmap (\x -> [| long $(mkStr $ q x) |]) . _optDesc   -- TODO: switch from 'long' to 'desc'
                        where
                            q :: FmtMap ST -> String
                            q x = case Map.lookup defaultFmt x of Just desc -> cs desc

    body <- [| $(return $ ConE dataName) <$> $(foldApQExps (varE $ mkName "<*>") fields) |]

    return $ FunD funName [Clause [] (NormalB body) []]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

(<$$>) :: (Applicative a, Applicative b) => (x -> y) -> a (b x) -> a (b y)
(<$$>) = fmap . fmap

-- | Intercalate a list of expressions with an infix operator.
foldApQExps :: Q Exp -> [Q Exp] -> Q Exp
foldApQExps infx l@(_:_) = foldr1 (\ a b -> UInfixE <$> a <*> infx <*> b) l

titleCase :: String -> String
titleCase "" = ""
titleCase (c:cs) = toUpper c : cs

untitleCase :: String -> String
untitleCase "" = ""
untitleCase (c:cs) = toLower c : cs
--------------
-- for ghci --
--------------

ios :: IO String
ios = decodeFile "example.yaml" >>= writer . fromJust

writer :: GarnettFile -> IO String  
writer gf = runQ $ pprint <$> splice gf 
