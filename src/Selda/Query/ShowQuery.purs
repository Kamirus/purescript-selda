module Selda.Query.ShowQuery where

import Prelude

import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..), maybe)
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

ishowLimitOffset ∷ Maybe Int → Maybe Int → String
ishowLimitOffset limit offset = case offset of
  Just o | o > 0 →
    let l = maybe (top ∷ Int) (max 0) limit in
    "LIMIT " <> show l <> " OFFSET " <> show o
  _ → case limit of
    Nothing → ""
    Just l → "LIMIT " <> show (max 0 l)

ishowAliasedCol ∷ Tuple Alias (Exists Expr) → ShowM
ishowAliasedCol (Tuple alias ee) = do
  s ← runExists showExpr ee
  pure $ s <> " AS " <> alias
