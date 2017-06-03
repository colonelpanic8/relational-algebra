{-|
Module      : Select
Description : AST for top level Select queries
Copyright   : (c) LeapYear Technologies 2016
Maintainer  : eitan@leapyear.io
Stability   : experimental
Portability : POSIX

ASTs for Selects
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Select
  ( Select(..)
  , SelectIdentifier
  , PredicateException(..)
  , execute
  , trim
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Data.Typeable
import Debug.Trace
import Pipes
import Pipes.Prelude (toList)
import Text.CSV
import Unsafe.Coerce

import Select.Relation
import Select.Expression

-- | Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)

-- | Identifier for SELECT
type SelectIdentifier = Select String String FilePath

trim :: String -> String
trim = f . f
f = reverse . dropWhile isSpace

data PredicateException = PredicateException deriving (Show, Typeable)

instance Exception PredicateException

convertFilepathToTable :: FilePath -> IO CSVTable
convertFilepathToTable path = do
  csvString <- readFile path
  let Right res = parseCSV path $ trim csvString
  return $ CSVTable res

-- | `execute` should take in a Select AST and a CSV filename for output
-- and execute the select statement to create a new tabular dataset which
-- it will populate the output file with
execute
  :: SelectIdentifier
  -> FilePath -- output
  -> IO ()
execute (SELECT rel) output = do
  converted <- convertRelation convertFilepathToTable rel
  let p = relationToRowProducer [] converted
  case p of
    Right producer -> do
      let finalTable = toList $ rowProducer producer
          showHack v@(Value s) = if typeOfValue s == stringType
                                 then unsafeCoerce s
                                 else show v
          csvObj = (map fst $ columnTypes producer):map (map showHack) finalTable
          makeLine = intercalate ","
          csvString = intercalate "\n" $ map makeLine csvObj
      -- writeFile output $ show finalTable
      writeFile output $ csvString
    Left e -> writeFile output $ show e
  return ()


data CSVTable = CSVTable [[String]]
stringType = AnyType (Type :: Type String)

instance BuildsRowProducer CSVTable Identity String where
  getRowProducer (CSVTable table) reqs =
    let actualNames = head table
        requiredNames = map fst reqs
        getTypeForName name = fromMaybe stringType $ lookup name reqs
        getPairForName name = (name, getTypeForName name)
        typedNames = map getPairForName actualNames
        types = trace (show requiredNames) $ map snd typedNames
        tryRead at@(AnyType t) s =
          if at == stringType
            then Right $ Value s
            else maybe (Left $ BadValueError s) (Right . Value) $ readAsType t s
        typeRow row = zipWithM tryRead types row
        typedTableOrError = mapM typeRow $ tail table
        makeProducer valueTable =
          Right $
          TypedRowProducer
          {columnTypes = typedNames, rowProducer = each valueTable}
    in if all (flip elem actualNames) requiredNames
         then typedTableOrError >>= makeProducer
         else Left BadExpressionError -- TODO: give some info here
