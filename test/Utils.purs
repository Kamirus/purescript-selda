module Test.Utils where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.Array (fromFoldable)
import Data.Either (either)
import Data.Foldable (class Foldable, find, foldl, for_, length)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Variant.Internal (FProxy(..))
import Database.PostgreSQL (Connection, PGError)
import Effect.Aff (Aff, catchError, throwError)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign (ForeignError, MultipleErrors, renderForeignError)
import Global.Unsafe (unsafeStringify)
import SQLite3 (DBConnection)
import Selda (FullQuery, showQuery)
import Selda.Col (class GetCols)
import Selda.Expr (ShowM)
import Selda.PG (showPG)
import Selda.PG.Class (BackendPGClass)
import Selda.Query.Class (class GenericQuery, genericQuery, runSelda)
import Selda.SQLite3 (showSQLite3)
import Selda.SQLite3.Class (BackendSQLite3Class)
import Test.Unit (TestSuite)
import Test.Unit as Unit
import Test.Unit.Assert (assert)
import Type.Proxy (Proxy(..))

type TestCtx b m ctx =
  { b ∷ Proxy b
  , m ∷ FProxy m
  , ctx ∷ ctx
  }

class TestBackend b m ctx | b m → ctx where
  testWith
    ∷ ∀ i o
    . GenericQuery b m i o
    ⇒ GetCols i
    ⇒ TestCtx b m ctx
    → (Array { | o } → Array { | o } → Aff Unit)
    → String
    → Array { | o }
    → FullQuery Unit { | i }
    → TestSuite

instance testBackendPG
    ∷ TestBackend BackendPGClass
        (ExceptT PGError (ReaderT Connection Aff))
        { conn ∷ Connection }
  where
  testWith { b, m, ctx: { conn } } assertFn  = do
    testWith_ assertFn (showPG >>> _.strQuery) b (runPGSeldaAff conn)

instance testBackendSQLite3
    ∷ TestBackend BackendSQLite3Class
        (ExceptT (NonEmptyList ForeignError) (ReaderT DBConnection Aff))
        { conn ∷ DBConnection }
  where
  testWith { b, m, ctx: { conn } } assertFn = do
    testWith_ assertFn (showSQLite3 >>> _.strQuery) b (runSQLite3SeldaAff conn)

testWith_
  ∷ ∀ m i o b
  . GenericQuery b m i o
  ⇒ GetCols i
  ⇒ (Array { | o } → Array { | o } → Aff Unit)
  → (ShowM → String)
  → Proxy b
  → (m ~> Aff)
  → String
  → Array { | o }
  → FullQuery Unit { | i }
  → TestSuite
testWith_ assertFn showB b runM  = 
  testQueryWith_ (\q → runM $ genericQuery b q) assertFn (showQuery >>> showB)

testWithPG
  ∷ ∀ a
  . Connection
  → (TestCtx BackendPGClass PGSelda { conn ∷ Connection } → a)
  → a
testWithPG conn k = k { b, m, ctx: { conn } } 
  where
    b = (Proxy ∷ Proxy BackendPGClass)
    m = (FProxy ∷ FProxy PGSelda)

testWithSQLite3
  ∷ ∀ a
  . DBConnection
  → (TestCtx BackendSQLite3Class SQLite3Selda { conn ∷ DBConnection } → a)
  → a
testWithSQLite3 conn k = k { b, m, ctx: { conn } } 
  where
    b = (Proxy ∷ Proxy BackendSQLite3Class)
    m = (FProxy ∷ FProxy SQLite3Selda)

testQueryWith_
  ∷ ∀ expected query queryResult
  . (query → Aff queryResult)
  → (expected → queryResult → Aff Unit)
  → (query → String)
  → String
  → expected
  → query
  → TestSuite
testQueryWith_ run assertFunc showQ msg expected query =
  Unit.test msg do
    run query >>= assertFunc expected # catchError $ \e → do
      log "Error occured - Printing the query below"
      log $ showQ query
      throwError e

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

runPGSeldaAff ∷ Connection → PGSelda ~> Aff
runPGSeldaAff = runSeldaAffWith $ show

runSQLite3SeldaAff ∷ DBConnection → SQLite3Selda ~> Aff
runSQLite3SeldaAff = runSeldaAffWith $ show <<< map renderForeignError

type SQLite3Selda = ExceptT MultipleErrors (ReaderT DBConnection Aff)

type PGSelda = ExceptT PGError (ReaderT Connection Aff)

assertIn ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertIn l1 l2 = for_ l1 \x1 → do
  case find (x1 == _) l2 of
    Nothing → assert ((show x1) <> " not found in [" <> foldl (\acc x → acc <> show x <> " ") " " l2 <> "]") false
    Just _ → pure unit

assertUnorderedSeqEq ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertUnorderedSeqEq l1 l2 = do
  assertIn l1 l2
  assertIn l2 l1
  let 
    len1 = (length l1 ∷ Int)
    len2 = (length l2 ∷ Int)
    msg = "The same elements, but the length is different : " <> show len1 <> " != " <> show len2
  assert msg $ len1 == len2

assertSeqEq ∷ ∀ f2 f1 a. Show a ⇒ Eq a ⇒ Foldable f2 ⇒ Foldable f1 ⇒ f1 a → f2 a → Aff Unit
assertSeqEq l1 l2 = assert msg $ xs == ys
  where
  msg = show xs <> " != " <> show ys
  xs = fromFoldable l1
  ys = fromFoldable l2

assertEq ∷ ∀ a. Show a ⇒ Eq a ⇒ a → a → Aff Unit
assertEq x y = assert msg $ x == y
  where msg = show x <> " != " <> show y
