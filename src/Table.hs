{-|
Module      : Table
Description : Internal representation of a table
Copyright   : (c) Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Table
  ( evaluateRelation
  , outputValue
  , Table(..)
  , TableError(..)
  , TableValue(..)
  ) where

import Data.Maybe
import Data.List
import Select.Expression
import Select.Relation
import Text.Printf
import Text.Read
