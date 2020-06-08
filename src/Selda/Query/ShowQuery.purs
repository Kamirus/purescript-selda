module Selda.Query.ShowQuery where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr, ShowM, showExpr)
import Selda.Query.Type (Order(..), QBinOp(..))
import Selda.Table (Alias)

showCompoundOp ∷ QBinOp → String
showCompoundOp = case _ of
  Union → " UNION "
  UnionAll → " UNION ALL "
  Intersect → " INTERSECT "
  Except → " EXCEPT "

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

showHavings ∷ Array (Expr Boolean) → ShowM
showHavings = showXS " HAVING " " AND " showExpr

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
