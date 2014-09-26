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

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Conversions
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Options.Applicative
import Text.Garnett.Definition


writer :: GarnettFile -> IO String  -- should this be @Text.PrettyPrint.Free.Doc e@?
writer gf = runQ $ (intercalate "\n" <$>) . sequence $ pprint <$$> [buildType (_mainParser gf), buildFun (_mainParser gf)]

(<$$>) :: (Applicative a, Applicative b) => (x -> y) -> a (b x) -> a (b y)
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
    let name_    = camelizeCap . cs $ _parserName gparser
        dataName = mkName name_
        funName  = mkName . ("parseOpt" <>) $ name_

        fields :: [Q Exp]
        fields | length v /= 0 = v
          where
            v = map f $ _options gparser

            f :: Option -> Q Exp
            f option = [| argument auto $(foldApQExps $ catMaybes [_long option, _short option]) |]
                where
                    _long :: Option -> Maybe (Q Exp)
                    _long = fmap (\x -> [| long $(return . VarE . mkName . cs . show $ x) |]) . _longOpt

                    _short :: Option -> Maybe (Q Exp)
                    _short = fmap (\x -> [| short $(return . VarE . mkName . show $ [x]) |]) . _shortOpt

--                    _help :: Option -> Maybe (Q Exp)
--                    _help = fmap (\x -> [| short $(return . VarE . mkName $ lkup "default" x) |]) . _optDesc

    body <- [| $(return $ VarE dataName) <$> $(foldApQExps fields) |]

    return $ FunD funName [Clause [] (NormalB body) []]


-- | Intercalate a list of expressions with @<*>@.  TODO: find a
-- better name, comment better.
foldApQExps :: [Q Exp] -> Q Exp
foldApQExps = foldr1 $ \ a b -> UInfixE <$> a <*> [| (<*>) |] <*> b

camelizeCap :: String -> String
camelizeCap "" = ""
camelizeCap (c:cs) = toUpper c : cs

camelizeSmall :: String -> String
camelizeSmall "" = ""
camelizeSmall (c:cs) = toLower c : cs
