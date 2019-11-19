module Selda.Query.ShowQuery where

import Prelude

import Data.Array (reverse)
import Data.Array as Array
import Data.Exists (Exists, runExists)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr, ShowM, showExpr)
import Selda.Query.Type (GenState, Order(..), SQL(..), Source(..))
import Selda.Table (Alias)

showState ∷ GenState → ShowM
showState { cols, sources, restricts, aggr, order, limit } = 
  let appendM a b = (<>) <$> a <*> b in
  showCols cols
    `appendM` showSources sources
    `appendM` showRestricts restricts
    `appendM` showGrouping aggr
    `appendM` showOrdering order
    `appendM` (showLimit >>> pure) limit

showSources ∷ Array Source → ShowM
showSources sources = case Array.uncons $ reverse sources of
  Nothing → pure ""
  Just { head, tail } → do 
    let
      f acc x = do
        src ← showSource x
        pure $ acc <> sepFor x <> src
    h ← showSource head
    (<>) " FROM " <$> foldM f h tail

showSource ∷ Source → ShowM
showSource = case _ of
  Product t → showSQL t
  LeftJoin t e → do
    sql ← showSQL t
    exp ← showExpr e
    pure $ sql <> " ON (" <> exp <> ")"

showSQL ∷ SQL → ShowM
showSQL = case _ of
  FromTable t → pure $ t.name <> " " <> t.alias
  SubQuery alias state → do
    s ← showState state
    pure $ "(" <> s <> ") " <> alias

showXS ∷ ∀ a m. Monad m ⇒ String → String → (a → m String) → Array a → m String
showXS clause sep f = case _ of
  [] → pure ""
  xs → do
    ss ← traverse f xs
    pure $ clause <> joinWith sep ss

showCols ∷ Array (Tuple Alias (Exists Expr)) → ShowM
showCols = showXS "SELECT " ", " showAliasedCol

showRestricts ∷ Array (Expr Boolean) → ShowM
showRestricts = showXS " WHERE " " AND " showExpr

showGrouping ∷ Array (Exists Expr) → ShowM
showGrouping = showXS " GROUP BY " ", " $ runExists showExpr

showOrdering ∷ Array (Tuple Order (Exists Expr)) → ShowM
showOrdering = showXS " ORDER BY " ", " showOrder

showOrder ∷ Tuple Order (Exists Expr) → ShowM
showOrder (Tuple order e) = do
  s ← runExists showExpr e
  pure $ s <> " "
    <> case order of
      Asc → "ASC"
      Desc → "DESC"

showLimit ∷ Maybe Int → String
showLimit = case _ of
  Nothing → ""
  Just i → " LIMIT " <> (show $ max 0 i)

sepFor ∷ Source → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showAliasedCol ∷ Tuple Alias (Exists Expr) → ShowM
showAliasedCol (Tuple alias ee) = do
  s ← runExists showExpr ee
  pure $ s <> " AS " <> alias
