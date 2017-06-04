module Main (main) where

import Select
import Select.Expression
import Select.Relation
import System.IO
import Test.Hspec

main :: IO ()
main = hspec $

  describe "execute" $ do

    it "can select a table" $ do
      execute selectTable "output_people.csv"
      "output_people.csv" `shouldHaveSameContentAs` "data/people.csv"

    it "performs joins correctly" $ do
      execute selectJoin "output_join.csv"
      "output_join.csv" `shouldHaveSameContentAs` "data/expected_output_join.csv"

    it "performs filtering and comparisons correctly" $ do
      execute selectName "output_name.csv"
      "output_name.csv" `shouldHaveSameContentAs` "data/expected_output_name.csv"

    it "performs unions correctly" $ do
      execute selectJoinUnion "output_name2.csv"
      "output_name2.csv" `shouldHaveSameContentAs` "data/expected_output_union_join.csv"

    it "handles type errors" $ do
      execute selectAdditionAndTypeErrors "mismatch_output.csv"

selectTable :: SelectIdentifier
selectTable = SELECT (TABLE "data/people.csv")

selectName :: SelectIdentifier
selectName =
  SELECT $ [(sColumn "first_name") `as` "name"]
    `FROM` TABLE "data/people.csv" `WHERE`
       (iColumn "age" `Gte` Literal 40)

selectJoin :: SelectIdentifier
selectJoin = SELECT $
  [ iColumn "orders.order_id" `as` "order_id"
  , sColumn "customers.customer_name" `as` "customer_name"
  ] `FROM`
  ( TABLE "data/orders_table.csv" `AS` "orders"
    `INNER_JOIN_ON`
    TABLE "data/customer_table.csv" `AS` "customers"
  ) (sColumn ("orders", "customer_id") `Equ` Column ("customers", "customer_id"))

selectJoinUnion :: SelectIdentifier
selectJoinUnion = SELECT $
  [ iColumn "orders.order_id" `as` "order_id"
  , sColumn "customers.customer_name" `as` "customer_name"
  ] `FROM`
  ( (TABLE "data/orders_table.csv" `UNION` TABLE "data/other_orders_table.csv") `AS` "orders"
    `INNER_JOIN_ON`
    TABLE "data/customer_table.csv" `AS` "customers"
  ) (sColumn ("orders","customer_id") `Equ` Column ("customers","customer_id"))

selectPredicateError :: SelectIdentifier
selectPredicateError =
  SELECT $ TABLE "data/contains_type_mismatch.csv"
           `WHERE`
           iColumn "int_column" `Add` Column "double_column" `Gt` Literal 27

selectAdditionAndTypeErrors :: SelectIdentifier
selectAdditionAndTypeErrors = SELECT $
  [ iColumn "int_column" `Add` Column "double_column" `as` "added" ]
  `FROM`
   TABLE "data/contains_type_mismatch.csv"

shouldHaveSameContentAs :: FilePath -> FilePath -> Expectation
file1 `shouldHaveSameContentAs` file2 =
  withFile file1 ReadMode $ \handle1 ->
    withFile file2 ReadMode $ \handle2 -> do
      contents1 <- hGetContents handle1
      contents2 <- hGetContents handle2
      trim contents1 `shouldBe` trim contents2
