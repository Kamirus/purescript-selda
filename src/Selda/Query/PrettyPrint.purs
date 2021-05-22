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
import Data.Exists (Exists, runExists)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Dodo (Doc, alignCurrentColumn, appendBreak, break, foldWithSeparator, indent, isEmpty, lines, plainText, print, text, twoSpaces)
import Foreign (Foreign)
import Selda.Expr (Expr, QueryParams, ShowMCtx, showExpr, showM)
import Selda.Query.ShowQuery (ishowAliasedCol, ishowCompoundOp, ishowJoinType, ishowLimit, ishowOrder)
import Selda.Query.Type (GenState_, Order, SQL(..), Source(..))
import Selda.Table (Alias)

type PrettyM = ReaderT ShowMCtx (State QueryParams) (Doc String)

prettyM
  ∷ String
  → Int
  → PrettyM
  → { params ∷ Array Foreign, nextIndex ∷ Int, strQuery ∷ String }
prettyM ph i m = showM ph i $ dodoPrint <$> m

ppState ∷ GenState_ → PrettyM
ppState { cols, source, restricts, havings, aggr, order, limit, distinct } =
  ppCols distinct cols
    `appDoc` ppFrom source
    `appTxt` ppRestricts restricts
    `appTxt` ppGrouping aggr
    `appTxt` ppHavings havings
    `appTxt` ppOrdering order
    `appTxt` (pure <<< text <<< ishowLimit) limit
    where
      appDoc = lift2 appendBreak
      appTxt = lift2 appendNonEmptyDoc
      appendNonEmptyDoc a b =
        if isEmpty b
        then a
        else a <> break <> b

ppCols ∷ Boolean → Array (Tuple Alias (Exists Expr)) → PrettyM
ppCols distinct = ppXS clause commaBreak (map text <<< ishowAliasedCol)
  where
    clause = "SELECT " <> if distinct then "DISTINCT " else ""

ppFrom ∷ Source → PrettyM
ppFrom source = (text "FROM " <> _) <$> ppSource source

ppSource ∷ Source → PrettyM
ppSource = case _ of
  From t → ppSQL t
  CrossJoin src sql → do
    src' ← ppSource src
    sql' ← ppSQL sql
    pure $ src' <> break <> text "CROSS JOIN " <> sql'
  JoinOn joinType src sql e → do
    src' ← ppSource src
    sql' ← ppSQL sql
    e' ← showExpr e
    pure $ src' <> break <> 
      text (ishowJoinType joinType) <> sql' <> text (" ON (" <> e' <> ")")
  Combination op q1 q2 alias → do
    s1 ← ppState $ unwrap q1
    s2 ← ppState $ unwrap q2
    let
      ppCombinedSubQuery s = lines
        [ text "SELECT * FROM"
        , text "(" <> alignCurrentColumn s
        , text ") combined_sub_query"
        ]
    pure $ indent $ (break <> _) $ lines
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
      <> text "(" <> alignCurrentColumn s <> break <> text (") " <> alias)

ppRestricts ∷ Array (Expr Boolean) → PrettyM
ppRestricts = ppXS "WHERE " (break <> text "AND ") ppExpr

ppHavings ∷ Array (Expr Boolean) → PrettyM
ppHavings = ppXS "HAVING " (break <> text "AND ") ppExpr

ppGrouping ∷ Array (Exists Expr) → PrettyM
ppGrouping = ppXS "GROUP BY " commaBreak $ runExists ppExpr

ppOrdering ∷ Array (Tuple Order (Exists Expr)) → PrettyM
ppOrdering = ppXS "ORDER BY " commaBreak (map text <<< ishowOrder)

dodoPrint ∷ ∀ a. Doc a → String
dodoPrint = print plainText twoSpaces

ppExpr ∷ ∀ a. Expr a → PrettyM
ppExpr e = text <$> showExpr e

commaBreak ∷ ∀ a. Doc a
commaBreak = text "," <> break

ppXS ∷ ∀ a d m. Monad m ⇒ String → Doc d → (a → m (Doc d)) → Array a → m (Doc d)
ppXS clause sep f = case _ of
  [] → pure $ mempty
  xs → do
    ss ← traverse f xs
    pure $ text clause
      <> alignCurrentColumn (foldWithSeparator sep ss)
