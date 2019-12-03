module Test.Utils where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Array (fromFoldable)
import Data.Either (either)
import Data.Foldable (class Foldable, find, foldl, for_)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL as PostgreSQL
import Effect.Aff (Aff, catchError, throwError)
import Effect.Exception (error)
import Foreign (MultipleErrors, renderForeignError)
import Global.Unsafe (unsafeStringify)
import SQLite3 (DBConnection)
import Selda.Query.Class (runSelda)
import Test.Unit (TestSuite)
import Test.Unit as Unit
import Test.Unit.Assert (assert)

testQueryWith_
  ∷ ∀ expected query queryResult
  . (query → Aff queryResult)
  → (expected → queryResult → Aff Unit)
  → String
  → expected
  → query
  → TestSuite
testQueryWith_ run assertFunc msg expected query =
  Unit.test msg $ run query >>= assertFunc expected

testQuery
  ∷ ∀ a query
  . Show a ⇒ Eq a
  ⇒ (query → Aff (Array a))
  → String
  → Array a
  → query
  → TestSuite
testQuery execQuery = testQueryWith_ execQuery assertUnorderedSeqEq

withRollback_
  ∷ ∀ err a
  . (String → Aff (Maybe err))
  → Aff a
  → Aff Unit
withRollback_ exec action = do
  let
    throwErr msg err = throwError $ error $ msg <> unsafeStringify err
    rollback = exec "ROLLBACK" >>= case _ of
      Just err → throwErr "Error on transaction rollback: " err
      Nothing → pure unit
  begun ← exec "BEGIN TRANSACTION"
  case begun of
    Just err → throwErr "Error on transaction initialization: " err
    Nothing → void $ catchError
      (action >>= const rollback) (\e → rollback >>= const (throwError e))

runSeldaAff ∷ ∀ r e. r → ExceptT e (ReaderT r Aff) ~> Aff
runSeldaAff = runSeldaAffWith unsafeStringify

runSeldaAffWith ∷ ∀ e r. (e → String) → r → ExceptT e (ReaderT r Aff) ~> Aff
runSeldaAffWith fe conn m = runSelda conn m >>= either onError pure
  where
    msg = "Error occured during text execution: "
    onError e = throwError $ error $ msg <> fe e

runPGSeldaAff ∷ PostgreSQL.Connection → PGSelda ~> Aff
runPGSeldaAff = runSeldaAffWith $ show

runSQLite3SeldaAff ∷ DBConnection → SQLite3Selda ~> Aff
runSQLite3SeldaAff = runSeldaAffWith $ show <<< map renderForeignError

type SQLite3Selda = ExceptT MultipleErrors (ReaderT DBConnection Aff)

type PGSelda = ExceptT PostgreSQL.PGError (ReaderT PostgreSQL.Connection Aff)

assertIn ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertIn l1 l2 = for_ l1 \x1 → do
  case find (x1 == _) l2 of
    Nothing → assert ((show x1) <> " not found in [" <> foldl (\acc x → acc <> show x <> " ") " " l2 <> "]") false
    Just _ → pure unit

assertUnorderedSeqEq ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertUnorderedSeqEq l1 l2 = do
  assertIn l1 l2
  assertIn l2 l1

assertSeqEq ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertSeqEq l1 l2 = assert msg $ xs == ys
  where
  msg = show xs <> " != " <> show ys
  xs = fromFoldable l1
  ys = fromFoldable l2
