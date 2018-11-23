module Selda.Query
  ( restrict
  , select
  , leftJoin
  , leftJoin'
  , WrapWithMaybe
  , RenameNamespace
  ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array ((:))
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Selda.Col (class ExtractCols, class ToCols, Col(..), getCols, toCols)
import Selda.Expr (BinExp(..), Expr(..))
import Selda.Inner (Inner, OuterCols, outer)
import Selda.Query.Type (Query(..), SQL(..), Source(..), freshId, runQuery)
import Selda.Table (class TableColumns, Alias, Column(..), Table(..), tableColumns)
import Type.Proxy (Proxy(..))
import Type.Row (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

restrict ∷ ∀ s. Col s Boolean → Query s Unit
restrict (Col e) = Query do
  st ← get
  put $ st { restricts = e : st.restricts }

select
  ∷ ∀ s r rl res i il
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ Table r → Query s (Record res)
select table = do
  { res, sql } ← fromTable table
  st ← Query get
  Query $ put $ st { sources = Product sql : st.sources }
  pure res

leftJoin
  ∷ ∀ r s res rl il i mres
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ Table r → (Record res → Col s Boolean) → Query s (Record mres)
leftJoin table on = do
  { res, sql } ← fromTable table
  let Col e = on res
  st ← Query get
  Query $ put $ st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

-- | `leftJoin' on q`
-- | run sub query `q`;
-- | rename namespaces of columns in its result;
-- | with this execute `on` to get JOIN constraint;
-- | add sub query to sources;
-- | return previously mapped record with each value in Col wrapped in Maybe
-- | (because LEFT JOIN can return null for each column)
leftJoin'
  ∷ ∀ s res res0 rl mres inner
  . HMap OuterCols (Record inner) (Record res0)
  ⇒ HMap RenameNamespace (Record res0) (Record res)
  ⇒ HMap WrapWithMaybe (Record res) (Record mres)
  ⇒ RL.RowToList res0 rl ⇒ ExtractCols res0 rl
  ⇒ (Record res → Col s Boolean)
  → Query (Inner s) (Record inner)
  → Query s (Record mres)
leftJoin' on q = do
  { res, sql, alias } ← fromSubQuery q
  let Col e = on res
  st ← Query get
  Query $ put $ st { sources = LeftJoin sql e : st.sources }
  pure $ hmap WrapWithMaybe res

fromTable
  ∷ ∀ r s res rl il i
  . RL.RowToList r rl ⇒ TableColumns rl i ⇒ RL.RowToList i il ⇒ ToCols s i il res
  ⇒ Table r → Query s { res ∷ Record res , sql ∷ SQL }
fromTable (Table { name }) = do
  id ← freshId
  st ← Query get
  let
    aliased = { name, alias: name <> "_" <> show id }
    i = tableColumns aliased (RLProxy ∷ RLProxy rl)
    res = toCols (Proxy ∷ Proxy s) i (RLProxy ∷ RLProxy il)
  pure $ { res, sql: FromTable aliased }

data WrapWithMaybe = WrapWithMaybe
instance wrapWithMaybeInstance
    ∷ Mapping WrapWithMaybe (Col s a) (Col s (Maybe a))
  where
  mapping WrapWithMaybe = (unsafeCoerce ∷ Col s a → Col s (Maybe a))

newtype RenameNamespace = RenameNamespace Alias
instance renameNamespaceInstance
    ∷ Mapping RenameNamespace (Col s a) (Col s a)
  where
  mapping (RenameNamespace s) (Col e) = Col $ renameNamespace s e

subQueryAlias ∷ ∀ s. Query s Alias
subQueryAlias = do
  id ← freshId
  pure $ "sub" <> "_q" <> show id

fromSubQuery
  ∷ ∀ inner s rl res res0
  . HMap OuterCols (Record inner) (Record res0)
  ⇒ RL.RowToList res0 rl ⇒ ExtractCols res0 rl
  ⇒ HMap RenameNamespace (Record res0) (Record res)
  ⇒ Query (Inner s) (Record inner)
  → Query s { res ∷ Record res , sql ∷ SQL , alias ∷ Alias }
fromSubQuery q = do
  let (Tuple innerRes st) = runQuery q
  let res0 = outer innerRes
  let cols = getCols res0
  alias ← subQueryAlias
  let res = hmap (RenameNamespace alias) res0
  pure $ { res, sql: SubQuery alias $ st { cols = getCols res0 }, alias }

-- | `renameNamespace namespace expr`
-- | change namespace in every column in `expr`
-- | When namespace changes?
-- | Columns outside of subqueries are namespaced with subquery alias.
-- | select aux.id from (select id from people ...) aux ...
renameNamespace ∷ ∀ a. Alias → Expr a → Expr a
renameNamespace namespace = rename
  where
  rename ∷ ∀ o. Expr o → Expr o
  rename e = case e of
    EColumn (Column c) → EColumn $ Column $ c { namespace = namespace }
    ELit _ → e
    EBinOp ebinop → EBinOp $ runExists renameBinExp ebinop
  renameBinExp ∷ ∀ o i. BinExp o i → Exists (BinExp o)
  renameBinExp (BinExp op e1 e2) = mkExists $ BinExp op (rename e1) (rename e2)
