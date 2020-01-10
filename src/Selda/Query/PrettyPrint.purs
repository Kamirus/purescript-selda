module Selda.Query.PrettyPrint where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Newtype (unwrap)
import Foreign (Foreign)
import Selda.Expr (QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (showCols, showCompoundOp, showGrouping, showLimit, showOrdering, showRestricts)
import Selda.Query.Type (GenState_, SQL(..), Source(..))
import Text.Pretty (Doc, line, nest, render, text)

type PrettyM = ReaderT ShowMCtx (State QueryParams) (Doc String)

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ render 0 <$> m

ppState ∷ GenState_ → PrettyM
ppState { cols, source, restricts, aggr, order, limit, distinct } =
  (text <$> (<>) " " <$> showCols distinct cols)
    `appDoc` ((<>) (text " FROM ") <$> ppSource source)
    `appTxt` showRestricts restricts
    `appTxt` showGrouping aggr
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
  LeftJoin src sql e → do
    src' ← ppSource src
    sql' ← ppSQL sql
    e' ← showExpr e
    pure $ src' <> line <> 
      text " LEFT JOIN " <> sql' <> text (" ON (" <> e' <> ")")
  Combination op q1 q2 alias → do
    s1 ← ppState $ unwrap q1
    s2 ← ppState $ unwrap q2
    pure $ nest 2 $ line
      <> text "(" <> nest 1 s1 <> line
      <> text (showCompoundOp op) <> line
      <> text " " <> nest 1 s2 <> line
      <> text (") " <> alias)

ppSQL ∷ SQL → PrettyM
ppSQL = case _ of
  FromTable t → pure $ text $ t.name <> " " <> t.alias
  SubQuery alias state → do
    s ← ppState $ unwrap state
    pure $ nest 2 $ line <> text "(" <> nest 1 (s <> text (" ) " <> alias))
