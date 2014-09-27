{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -ddump-splices    #-}
{- |
    Module : Writers.HaskellWriter
    Copyright : Copyright (C) 2014 Julian K. Arni
    License : BSD3
    Maintainer : Julian K. Arni <jkarni@gmail.com>
    Stability : alpha

Tests for the HaskellWriter. Also doubles up as a working example of
a Garnett-using executable.
-}
module Writers.HaskellWriter where
       
import Options.Applicative
import Control.Monad
  
import Text.Garnett.Definition
import Text.Garnett.Writers.HaskellWriter

$(mkOptParser "example.yaml")

{-
data MainTy
  = MainTy {verbosity :: Bool,
            help :: Bool,
            inputFile :: [FilePath],
            logLevel :: Bool}

parseOptmain :: Parser MainTy
parseOptmain = (MainTy <$> (option auto ((long "verbose") <> short 'v'))
        <*>
          option
            auto
            ((long "help") <> short 'h')
        <*>
          option auto ((long "input") <> short 'i' )
        <*>
          option
            auto ((long "log") <> short 'l' ))
-}

entryPoint :: MainTy -> IO ()
entryPoint mn = do
  --when (verbosity mn) putStrLn "Verbose!"
  putStrLn "Input files:"
  mapM_ (\x -> putStrLn ('\t':x)) (inputFile mn)
  --when (logLevel mn) putStrLn "Logging!"
    

localMain :: IO ()
localMain = execParser opts >>= entryPoint
 where
    opts = info (helper <*> parseOptmain)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )