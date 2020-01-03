module Selda.Query.PrettyPrint where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Array (reverse)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Prettier.Printer (DOC, line, nest, pretty, text)
import Selda.Expr (QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (showCols, showGrouping, showLimit, showOrdering, showRestricts)
import Selda.Query.Type (GenState, SQL(..), Source(..))

type PrettyM = ReaderT ShowMCtx (State QueryParams) DOC

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ pretty 0 <$> m

ppState ∷ GenState → PrettyM
ppState { cols, sources, restricts, aggr, order, limit } =
  (text <$> (<>) " " <$> showCols cols)
    `appDoc` ppSources sources
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

ppSources ∷ Array Source → PrettyM
ppSources sources = case Array.uncons $ reverse sources of
  Nothing → pure mempty
  Just { head, tail } → do 
    let
      ppSourcesStep acc x = do
        src ← ppSource x
        pure $ acc <> line <> sepFor x <> src
    h ← ppSource head
    (<>) (text " FROM ") <$> foldM ppSourcesStep h tail

sepFor ∷ Source → DOC
sepFor = case _ of
  Product _ → text " CROSS JOIN "
  LeftJoin _ _ → text " LEFT JOIN "

ppSource ∷ Source → PrettyM
ppSource = case _ of
  Product t → ppSQL t
  LeftJoin t e → do
    sql ← ppSQL t
    exp ← showExpr e
    pure $ sql <> text (" ON (" <> exp <> ")")

ppSQL ∷ SQL → PrettyM
ppSQL = case _ of
  FromTable t → pure $ text $ t.name <> " " <> t.alias
  SubQuery alias state → do
    s ← ppState state
    pure $ nest 2 $ line <> text "(" <> nest 1 (s <> text (" ) " <> alias))
