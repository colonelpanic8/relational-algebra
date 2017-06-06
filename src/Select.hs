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
{-# LANGUAGE OverloadedStrings #-}

module Select
  ( Select(..)
  , SelectIdentifier
  , execute
  , trim
  ) where

import           Blaze.ByteString.Builder (toByteString, fromByteString)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Identity
import           Data.ByteString.Char8 (pack)
import           Data.Char (isSpace)
import qualified Data.Csv as CSV
import           Data.Dynamic
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Typeable
import qualified Data.Vector as V
import           Debug.Trace
import           Pipes hiding (Proxy)
import qualified Pipes.ByteString as PBS
import           Pipes.Csv
import           Pipes.Csv.Encoding
import qualified Pipes.Prelude as P
import           System.IO
import           Text.CSV
import           Text.Read
import           Unsafe.Coerce

import           Select.Expression
import           Select.Relation

-- | Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)

-- | Identifier for SELECT
type SelectIdentifier = Select String String FilePath

trim :: String -> String
trim = f . f
f = reverse . dropWhile isSpace

myEncode opts = P.map (helper (encDelimiter opts) . toRecord)
  where helper d = toByteString . (<> fromByteString "\n") . encodeRecord d

-- | `execute` should take in a Select AST and a CSV filename for output
-- and execute the select statement to create a new tabular dataset which
-- it will populate the output file with
execute
  :: SelectIdentifier
  -> FilePath -- output
  -> IO ()
execute (SELECT rel) output =
  withFile output WriteMode $ \outputHandle -> do
    converted <- convertRelation filepathToStreamingTable rel
    let p = relationToRowProducer [] converted
    case p of
      Left e -> trace (show e) $ return ()
      Right _ -> return ()
    let Right producer = p
        consumer = PBS.toHandle outputHandle
        header = intercalate "," $ map fst $ columnTypes producer
        encoder = myEncode defaultEncodeOptions
    hPutStr outputHandle $ header ++ "\n"
    runEffect $ rowProducer producer >-> encoder >-> consumer
    return ()

instance ToField Value where
  toField = pack . writeValue

filepathToStreamingTable :: FilePath -> IO (StreamingTable String)
filepathToStreamingTable path = do
  handle <- openFile path ReadMode
  line <- hGetLine handle
  let header = map trim $ splitOn "," line
  return $ StreamingTable header handle

data StreamingTable v = StreamingTable [v] Handle

data StreamingException = UnableToReadRow String | RelationException RelationError
    deriving (Show, Typeable)

instance Exception StreamingException

instance BuildsRowProducer (StreamingTable String) IO String where
  getRowProducer (StreamingTable actualNames handle) reqs =
    let requiredNames = map fst reqs
        getTypeForName name = fromMaybe stringSType $ lookup name reqs
        getPairForName name = (name, getTypeForName name)
        typedNames = map getPairForName actualNames
        types = map snd typedNames
        tryRead tr s =
          if tr == stringSType
            then Right $ Value s
            else maybe (Left $ BadValueError s) Right $ readValue tr s
        typeRow row = zipWithM tryRead types row
        handleCSVRow eRow =
          case eRow of
            Right row ->
              case typeRow row of
                Right typedRow -> yield typedRow
                Left e -> throw $ RelationException e
            Left e -> return $ throw $ UnableToReadRow e
        csvProducer :: Producer (Either String [String]) IO ()
        csvProducer = decode NoHeader $ PBS.fromHandle handle
        producer = for csvProducer handleCSVRow
    in if all (flip elem actualNames) requiredNames
         then Right $
              TypedRowProducer
              {columnTypes = typedNames, rowProducer = producer}
         else Left BadExpressionError -- TODO: give some info here
