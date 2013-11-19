module TestHelpers where

import Matching
import Pretty
import Syntax

import Backends.Haskell.Compiler
import Backends.Haskell.Syntax

import Control.Monad (liftM)

import Data.List (intersperse)

import Text.Parsec (many1,parse)
import Text.Parsec.String (parseFromFile)

import System.FilePath (takeBaseName)


-- HELPER


tolerant action successAction = do
  result <- action
  case result of
    Left err -> print err
    Right success -> successAction success

tolerantParseFile file = tolerant (parseFromFile iexpr file)

testParse file
  = tolerant (parseFromFile (many1 iexpr) file)
             (mapM_ putStrLn . intersperse "" . map (show . pretty))

tolerantRead = tolerant ((parse iexpr "<stdin>" . unlines) `liftM` getMultiLine)

testRead = tolerantRead prettyPrint

getLinesTill endmark = do
  line <- getLine
  if line == endmark
  then return []
  else (line:) `liftM` getLinesTill endmark

getMultiLine = getLinesTill ""


compileModule filename
  = tolerantParseFile filename $
      match iModule >>> eitherFailOr writeToFile
 where
   filebase = takeBaseName filename
   writeToFile = writeFile (filebase ++ ".hs") . show . prettyMod filebase

compileModulePrint filename
  = tolerantParseFile filename $
      match iModule >>> eitherFailOr (print . prettyMod filebase)
 where
   filebase = takeBaseName filename

compileRead compiler = tolerantRead $ match compiler >>> eitherFailOr prettyPrint

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = print . pretty


eitherFailOr f (Left err) = print err
eitherFailOr f (Right success) = f success

(f >>> g) x = g (f x)
