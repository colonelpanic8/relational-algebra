{-|
Module      : Select
Description : AST for top level Select queries
Copyright   : (c) LeapYear Technologies 2016
Maintainer  : eitan@leapyear.io
Stability   : experimental
Portability : POSIX

ASTs for Selects
-}

module Select
  ( Select(..)
  , SelectIdentifier
  , PredicateException(..)
  , execute
  , trim
  ) where

import Data.Char (isSpace)
import Data.List
import Data.Typeable
import Text.CSV
import Control.Exception

import Table
import Select.Relation
import Select.Expression

-- | Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)
  deriving (Eq,Show)

-- | Identifier for SELECT
type SelectIdentifier = Select String String FilePath

trim :: String -> String
trim = f . f
f = reverse . dropWhile isSpace

data PredicateException = PredicateException TableError deriving (Show, Typeable)

instance Exception PredicateException

-- | `execute` should take in a Select AST and a CSV filename for output
-- and execute the select statement to create a new tabular dataset which
-- it will populate the output file with
execute
  :: SelectIdentifier
  -> FilePath -- output
  -> IO ()
execute (SELECT rel) output = do
  r <- evaluateRelation <$> convertFilepathsToTables rel
  case r of
    Right result ->
      let csvObj = (columnNames result):map (map outputValue) (rows result)
          makeLine = intercalate ","
          csvString = intercalate "\n" $ map makeLine csvObj
      in
        writeFile output $ csvString
    Left e -> throw $ PredicateException e

convertFilepathToTable :: FilePath -> IO (Relation String String Table)
convertFilepathToTable path = do
  csvString <- readFile path
  let Right res = parseCSV path $ trim csvString
  return $ TABLE $ Table { rows = map (map AnyValue) $ tail res, columnNames = head res }

convertFilepathsToTables :: Relation String String FilePath -> IO (Relation String String Table)
convertFilepathsToTables (TABLE filepath) = convertFilepathToTable filepath
convertFilepathsToTables (FROM exps rel) =
  do
    updated <- convertFilepathsToTables rel
    return $ FROM exps updated
convertFilepathsToTables (WHERE rel exp) =
  do
    updated <- convertFilepathsToTables rel
    return $ WHERE updated exp
convertFilepathsToTables (UNION rel1 rel2) =
  do
    updated1 <- convertFilepathsToTables rel1
    updated2 <- convertFilepathsToTables rel2
    return $ UNION updated1 updated2
convertFilepathsToTables (INNER_JOIN_ON (AS rel1 name1) (AS rel2 name2) exp) =
  do
    updated1 <- convertFilepathsToTables rel1
    updated2 <- convertFilepathsToTables rel2
    return $ INNER_JOIN_ON (AS updated1 name1) (AS updated2 name2) exp
