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

ishowCompoundOp ∷ QBinOp → String
ishowCompoundOp = case _ of
  Union → " UNION "
  UnionAll → " UNION ALL "
  Intersect → " INTERSECT "
  Except → " EXCEPT "

ishowXS ∷ ∀ a m. Monad m ⇒ String → String → (a → m String) → Array a → m String
ishowXS clause sep f = case _ of
  [] → pure ""
  xs → do
    ss ← traverse f xs
    pure $ clause <> joinWith sep ss

ishowCols ∷ Boolean → Array (Tuple Alias (Exists Expr)) → ShowM
ishowCols distinct = ishowXS s ", " ishowAliasedCol
  where s = "SELECT " <> if distinct then "DISTINCT " else ""

ishowRestricts ∷ Array (Expr Boolean) → ShowM
ishowRestricts = ishowXS " WHERE " " AND " showExpr

ishowHavings ∷ Array (Expr Boolean) → ShowM
ishowHavings = ishowXS " HAVING " " AND " showExpr

ishowGrouping ∷ Array (Exists Expr) → ShowM
ishowGrouping = ishowXS " GROUP BY " ", " $ runExists showExpr

ishowOrdering ∷ Array (Tuple Order (Exists Expr)) → ShowM
ishowOrdering = ishowXS " ORDER BY " ", " ishowOrder

ishowOrder ∷ Tuple Order (Exists Expr) → ShowM
ishowOrder (Tuple order e) = do
  s ← runExists showExpr e
  pure $ s <> " "
    <> case order of
      Asc → "ASC"
      Desc → "DESC"

ishowLimit ∷ Maybe Int → String
ishowLimit = case _ of
  Nothing → ""
  Just i → " LIMIT " <> (show $ max 0 i)

ishowAliasedCol ∷ Tuple Alias (Exists Expr) → ShowM
ishowAliasedCol (Tuple alias ee) = do
  s ← runExists showExpr ee
  pure $ s <> " AS " <> alias
