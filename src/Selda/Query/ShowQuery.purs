module Selda.Query.ShowQuery where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr, ShowM, showExpr)
import Selda.Query.Type (GenState_, Order(..), QBinOp(..), SQL(..), Source(..))
import Selda.Table (Alias)

showState ∷ GenState_ → ShowM
showState { cols, source, restricts, aggr, order, limit, distinct } = 
  let appendM a b = (<>) <$> a <*> b in
  showCols distinct cols
    `appendM` ((<>) " FROM " <$> showSource source)
    `appendM` showRestricts restricts
    `appendM` showGrouping aggr
    `appendM` showOrdering order
    `appendM` (showLimit >>> pure) limit

showSource ∷ Source → ShowM
showSource = case _ of
  From t → showSQL t
  CrossJoin src sql → do
    src' ← showSource src
    sql' ← showSQL sql
    pure $ src' <> " CROSS JOIN " <> sql'
  LeftJoin src sql e → do
    src' ← showSource src
    sql' ← showSQL sql
    e' ← showExpr e
    pure $ src' <> " LEFT JOIN " <> sql' <> " ON (" <> e' <> ")"
  Combination op q1 q2 alias → do
    s1 ← showState $ unwrap q1
    s2 ← showState $ unwrap q2
    pure $ "(" <> s1 <> showCompoundOp op <> s2 <> ") " <> alias

showCompoundOp ∷ QBinOp → String
showCompoundOp = case _ of
  Union → " UNION "
  UnionAll → " UNION ALL "
  Intersect → " INTERSECT "
  Except → " EXCEPT "

showSQL ∷ SQL → ShowM
showSQL = case _ of
  FromTable t → pure $ t.name <> " " <> t.alias
  SubQuery alias state → do
    s ← showState $ unwrap state
    pure $ "(" <> s <> ") " <> alias

showXS ∷ ∀ a m. Monad m ⇒ String → String → (a → m String) → Array a → m String
showXS clause sep f = case _ of
  [] → pure ""
  xs → do
    ss ← traverse f xs
    pure $ clause <> joinWith sep ss

showCols ∷ Boolean → Array (Tuple Alias (Exists Expr)) → ShowM
showCols distinct = showXS s ", " showAliasedCol
  where s = "SELECT " <> if distinct then "DISTINCT " else ""

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

showAliasedCol ∷ Tuple Alias (Exists Expr) → ShowM
showAliasedCol (Tuple alias ee) = do
  s ← runExists showExpr ee
  pure $ s <> " AS " <> alias
