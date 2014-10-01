{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards      #-}
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
import           Data.Char
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as T
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
splice gf = sequence $ (buildFun $ _mainParser gf):(return <$> (buildTypeR $ _mainParser gf))

buildTypeR :: GParser -> [Dec]
buildTypeR m@(GParser { _subparsers = [], .. }) = buildType m
buildTypeR m@(GParser { .. }                 )  = concatMap buildTypeR _subparsers
                                               ++ buildType m

-- | Builds a data type declaration, with a single constructor and multiple
-- records, for a parser.
buildType :: GParser -> [Dec]
buildType gparser = catMaybes [ Just (DataD [] dataName [] [RecC dataName fields] [])
                              , mkSubparserType $ _subparsers gparser
                              ]
  where
    g :: Maybe OptionInputTy -> Type
    g Nothing            = ConT ''Bool
    g (Just OITInt)      = ConT ''Int
    g (Just OITFloat)    = ConT ''Float
    g (Just OITString)   = ConT ''String
    g (Just OITFile)     = ConT ''FilePath
    g (Just OITDir)      = ConT ''FilePath
    g (Just OITBool)     = ConT ''Bool
    g (Just (OITList t)) = AppT ListT (g (Just t))

    f :: Option -> (Name, Strict, Type)
    f opt = (mkName . cs $ _optName opt, NotStrict, g $ _input opt)

    subparserName, dataName :: Name
    subparserName = mkName $ "Subparsers" ++ T.unpack (_parserName gparser)
    dataName = mkDataName gparser

    mkSubparserType :: [GParser] -> Maybe Dec
    mkSubparserType []  = Nothing
    mkSubparserType gps = Just $
       DataD []
             subparserName
             []
             (map (\gp -> NormalC (mkName $ ("Mk" ++) . T.unpack $ _parserName gp)
                    [(NotStrict, ConT $ mkDataName gp)]) gps)
             []

    subparserField :: [GParser] -> Maybe VarStrictType
    subparserField [] = Nothing
    subparserField _  = Just (mkName "subparsers", NotStrict, ConT subparserName)

    fields = map f (_options gparser) ++ maybeToList (subparserField $ _subparsers gparser)



-- This function could use some cleaning up
buildFun :: GParser -> Q Dec
buildFun gparser = do
    let
        dataName = mkDataName gparser
        funName  = mkFunName gparser

        fields :: [Q Exp]
        fields | not (null v) = v
               | otherwise    = error "fields: expecting non-empty list"
          where
            v = map f $ _options gparser

            f :: Option -> Q Exp
            f opt | _input opt == Nothing      = [| switch $(foldrQExps (varE $ mkName "<>") _all) |]
                  | _input opt == Just OITBool = [| switch $(foldrQExps (varE $ mkName "<>") _all) |]
                  | otherwise                  = [| option auto $(foldrQExps (varE $ mkName "<>") _all) |]
                where
                    _all :: [Q Exp]
                    _all = catMaybes [ _long  opt
                                     , _short opt
                                     , _help  opt
                                     ]

                    mkStr :: String -> Q Exp
                    mkStr = return . LitE . StringL

                    mkChar :: Char -> Q Exp
                    mkChar = return . LitE . CharL

                    _long, _short, _help :: Option -> Maybe (Q Exp)
                    _long = fmap (\x -> [| long $(mkStr $ cs x) |]) . _longOpt

                    _short = fmap (\x -> [| short $(mkChar x) |]) . _shortOpt

                    _help o = do
                      mdesc <- _optDesc o
                      case q mdesc of
                               Just x -> Just [| help $(mkStr x) |]
                               Nothing -> Nothing
                        where
                            q :: FmtMap T.Text -> Maybe String
                            q x = T.unpack <$> (Map.lookup defaultFmt x <|> Map.lookup cliFmt x)

    body <- [| $(mkApplicative (conE dataName:fields)) |]

    return $ FunD funName [Clause [] (NormalB body) []]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

(<$$>) :: (Applicative a, Applicative b) => (x -> y) -> a (b x) -> a (b y)
(<$$>) = fmap . fmap

cliFmt :: Fmt
cliFmt = Fmt "cli"
-- | Intercalate a list of expressions with an infix operator (right-association).
foldrQExps :: Q Exp -> [Q Exp] -> Q Exp
foldrQExps infx = foldr1 (\ a b -> UInfixE <$> a <*> infx <*> b)

-- | Intercalate a list of expressions with an infix operator (left-association).
foldlQExps :: Q Exp -> [Q Exp] -> Q Exp
foldlQExps infx = foldl1 (\ a b -> UInfixE <$> a <*> infx <*> b)

-- | Put a list of expressions together using applicative style.
-- E.g.:
--   > x <- runQ (mkApplicative [ [| (+) |], [| Just 4 |], [| Just 3 |] ] )
--   > pprint x
--   >> (+) <$> Just 4 <*> Just 3
mkApplicative :: [Q Exp] -> Q Exp
mkApplicative [e,ex]      = UInfixE <$> e <*> varE (mkName "<$>") <*> ex
mkApplicative (e:se:exps) = foldlQExps (varE $ mkName "<*>") (inFmap:exps)
   where
     inFmap :: Q Exp
     inFmap = UInfixE <$> e <*> varE (mkName "<$>") <*> se
mkApplicative _           = error "mkApplicative: requires at least two list elements"

mkDataName :: GParser -> Name
mkDataName gp = case T.unpack (_parserName gp) of
    ""      -> mkName "Ty"
    (c:cs') -> mkName $ (++ "Ty") $ map san $ toUpper c : cs'
  where san '-' = '_'
        san x   = x

mkFunName :: GParser -> Name
mkFunName gp = case T.unpack (_parserName gp) of
    "" -> mkName "opt"
    x  -> mkName $ ("opt" ++) $ map san x
  where san '-' = '_'
        san x   = x
--------------
-- for ghci --
--------------

ios :: IO String
ios = decodeFile "example.yaml" >>= writer . fromJust

writer :: GarnettFile -> IO String
writer gf = runQ $ pprint <$> splice gf
