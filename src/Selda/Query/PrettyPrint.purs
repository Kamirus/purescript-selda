module Selda.Query.PrettyPrint where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Newtype (unwrap)
import Foreign (Foreign)
import Selda.Expr (QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (showCols, showCompoundOp, showGrouping, showHavings, showLimit, showOrdering, showRestricts)
import Selda.Query.Type (GenState_, JoinType(..), SQL(..), Source(..))
import Text.Pretty (Doc, line, nest, render, text)

type PrettyM = ReaderT ShowMCtx (State QueryParams) (Doc String)

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ render 0 <$> m

ppState ∷ GenState_ → PrettyM
ppState { cols, source, restricts, havings, aggr, order, limit, distinct } =
  (text <$> (<>) " " <$> showCols distinct cols)
    `appDoc` ((<>) (text " FROM ") <$> ppSource source)
    `appTxt` showRestricts restricts
    `appTxt` showGrouping aggr
    `appTxt` showHavings havings
    `appTxt` showOrdering order
    `appTxt` (showLimit >>> pure) limit
    where
      appDoc = lift2 \a b → a <> line <> b
      appTxt = lift2 appendNonEmptyText
      appendNonEmptyText a b =
        if b == mempty
        then a
        else a <> line <> text b

ppSource ∷ Source → PrettyM
ppSource = case _ of
  From t → ppSQL t
  CrossJoin src sql → do
    src' ← ppSource src
    sql' ← ppSQL sql
    pure $ src' <> line <> text " CROSS JOIN " <> sql'
  JoinOn joinType src sql e → do
    src' ← ppSource src
    sql' ← ppSQL sql
    e' ← showExpr e
    pure $ src' <> line <> 
      ppJoinType joinType <> sql' <> text (" ON (" <> e' <> ")")
  Combination op q1 q2 alias → do
    s1 ← ppState $ unwrap q1
    s2 ← ppState $ unwrap q2
    let
      ppCombinedSubQuery s = mempty
        <> text "SELECT * FROM" <> line 
        <> text "(" <> nest 1 s <> line
        <> text ") combined_sub_query"
    pure $ nest 2 $ line
      <> text "(" <> nest 1 (ppCombinedSubQuery s1) <> line
      <> text (showCompoundOp op) <> line
      <> text " " <> nest 1 (ppCombinedSubQuery s2) <> line
      <> text (") " <> alias)

ppSQL ∷ SQL → PrettyM
ppSQL = case _ of
  FromTable t → pure $ text t.body
  SubQuery alias state → do
    s ← ppState $ unwrap state
    pure $ nest 2 $ line <> text "(" <> nest 1 (s <> text (" ) " <> alias))

ppJoinType ∷ JoinType → Doc String
ppJoinType = text <<< case _ of
  LeftJoin → " LEFT JOIN "
  InnerJoin → " JOIN "
