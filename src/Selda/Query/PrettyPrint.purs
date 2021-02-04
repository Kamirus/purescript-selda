module Selda.Query.PrettyPrint
  ( PrettyM
  , prettyM
  , ppState
  , dodoPrint
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Newtype (unwrap)
import Dodo (Doc, alignCurrentColumn, break, indent, lines, plainText, print, text, twoSpaces)
import Foreign (Foreign)
import Selda.Expr (QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (ishowCols, ishowCompoundOp, ishowGrouping, ishowHavings, ishowLimit, ishowOrdering, ishowRestricts)
import Selda.Query.Type (GenState_, JoinType(..), SQL(..), Source(..))

type PrettyM = ReaderT ShowMCtx (State QueryParams) (Doc String)

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ dodoPrint <$> m

dodoPrint ∷ ∀ a. Doc a → String
dodoPrint = print plainText twoSpaces

ppState ∷ GenState_ → PrettyM
ppState { cols, source, restricts, havings, aggr, order, limit, distinct } =
  (text <$> (<>) " " <$> ishowCols distinct cols)
    `appDoc` ((<>) (text " FROM ") <$> ppSource source)
    `appTxt` ishowRestricts restricts
    `appTxt` ishowGrouping aggr
    `appTxt` ishowHavings havings
    `appTxt` ishowOrdering order
    `appTxt` (ishowLimit >>> pure) limit
    where
      appDoc = lift2 \a b → a <> break <> b
      appTxt = lift2 appendNonEmptyText
      appendNonEmptyText a b =
        if b == mempty
        then a
        else a <> break <> text b

ppSource ∷ Source → PrettyM
ppSource = case _ of
  From t → ppSQL t
  CrossJoin src sql → do
    src' ← ppSource src
    sql' ← ppSQL sql
    pure $ src' <> break <> text " CROSS JOIN " <> sql'
  JoinOn joinType src sql e → do
    src' ← ppSource src
    sql' ← ppSQL sql
    e' ← showExpr e
    pure $ src' <> break <> 
      ppJoinType joinType <> sql' <> text (" ON (" <> e' <> ")")
  Combination op q1 q2 alias → do
    s1 ← ppState $ unwrap q1
    s2 ← ppState $ unwrap q2
    let
      ppCombinedSubQuery s = lines
        [ text "SELECT * FROM"
        , text "(" <> alignCurrentColumn s
        , text ") combined_sub_query"
        ]
    pure $ indent $ lines
      [ text "(" <> alignCurrentColumn (ppCombinedSubQuery s1)
      , text (ishowCompoundOp op)
      , text " " <> alignCurrentColumn (ppCombinedSubQuery s2)
      , text (") " <> alias)
      ]

ppSQL ∷ SQL → PrettyM
ppSQL = case _ of
  FromTable t → pure $ text t.body
  SubQuery alias state → do
    s ← ppState $ unwrap state
    pure $ indent $ break
      <> text "(" <> alignCurrentColumn (s <> text (" ) " <> alias))

ppJoinType ∷ JoinType → Doc String
ppJoinType = text <<< case _ of
  LeftJoin → " LEFT JOIN "
  InnerJoin → " JOIN "
