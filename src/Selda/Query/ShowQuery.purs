module Selda.Query.ShowQuery where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr, ShowM, showExpr)
import Selda.Query.Type (JoinType(..), Order(..), QBinOp(..))
import Selda.Table (Alias)

ishowJoinType ∷ JoinType → String
ishowJoinType = case _ of
  LeftJoin → "LEFT JOIN "
  InnerJoin → "JOIN "

ishowCompoundOp ∷ QBinOp → String
ishowCompoundOp = case _ of
  Union → "UNION"
  UnionAll → "UNION ALL"
  Intersect → "INTERSECT"
  Except → "EXCEPT"

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
  Just i → "LIMIT " <> (show $ max 0 i)

ishowAliasedCol ∷ Tuple Alias (Exists Expr) → ShowM
ishowAliasedCol (Tuple alias ee) = do
  s ← runExists showExpr ee
  pure $ s <> " AS " <> alias
