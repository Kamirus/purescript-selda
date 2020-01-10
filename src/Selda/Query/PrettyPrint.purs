module Selda.Query.PrettyPrint where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Newtype (unwrap)
import Foreign (Foreign)
import Prettier.Printer (DOC, line, nest, pretty, text)
import Selda.Expr (QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (showCols, showCompoundOp, showGrouping, showLimit, showOrdering, showRestricts)
import Selda.Query.Type (GenState_, SQL(..), Source(..))

type PrettyM = ReaderT ShowMCtx (State QueryParams) DOC

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ pretty 0 <$> m

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
    pure $ mempty
      <> text "( (" <> nest 2 s1 <> text ")" <> line
      <> text (showCompoundOp op) <> line
      <> text "  (" <> nest 2 s2 <> text ")" <> line
      <> text (") " <> alias)

ppSQL ∷ SQL → PrettyM
ppSQL = case _ of
  FromTable t → pure $ text $ t.name <> " " <> t.alias
  SubQuery alias state → do
    s ← ppState $ unwrap state
    pure $ nest 2 $ line <> text "(" <> nest 1 (s <> text (" ) " <> alias))
