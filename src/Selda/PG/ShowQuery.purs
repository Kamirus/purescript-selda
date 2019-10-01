module Selda.PG.ShowQuery where

import Prelude

import Data.Array (foldl, reverse)
import Data.Array as Array
import Data.Exists (runExists)
import Data.Functor.Variant (VariantF)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Selda.Col (ExistsExpr)
import Selda.Query.Type (GenState, Order(..), SQL(..), Source(..))
import Selda.Table (Alias)

showState ∷ ∀ v. (∀ a. VariantF v a → String) → GenState v → String
showState showExpr { cols, sources, restricts, aggr, order, limit } = 
  showCols cols
    <> showSources sources
    <> showRestricts restricts
    <> showGrouping aggr
    <> showOrdering order
    <> showLimit limit
  where
  showCols ∷ Array (Tuple Alias (ExistsExpr v)) → String
  showCols = case _ of
    [] → ""
    xs → "SELECT " <> (joinWith ", " $ map showAliasedCol xs)

  showSources ∷ Array (Source v) → String
  showSources src = case Array.uncons $ reverse src of
    Nothing → ""
    Just { head, tail } → " FROM "
      <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource head) tail

  showRestricts ∷ Array (VariantF v Boolean) → String
  showRestricts = case _ of
    [] → ""
    xs → " WHERE " <> (joinWith " AND " $ map showExpr xs)

  showGrouping ∷ Array (ExistsExpr v) → String
  showGrouping = case _ of
    [] → ""
    xs → " GROUP BY " <> (joinWith ", " $ map (runExists showExpr) xs)

  showOrdering ∷ Array (Tuple Order (ExistsExpr v)) → String
  showOrdering = case _ of
    [] → ""
    xs → " ORDER BY " <> (joinWith ", " $ map showOrder xs)

  showOrder ∷ Tuple Order (ExistsExpr v) → String
  showOrder (Tuple ord e) =
    runExists showExpr e <> " "
      <> case ord of
        Asc → "ASC"
        Desc → "DESC"

  showLimit ∷ Maybe Int → String
  showLimit = case _ of
    Nothing → ""
    Just i → " LIMIT " <> (show $ max 0 i)

  showSQL ∷ SQL v → String
  showSQL = case _ of
    FromTable t →
      t.name <> " " <> t.alias
    SubQuery alias state → 
      "(" <> showState showExpr state <> ") " <> alias

  sepFor ∷ Source v → String
  sepFor = case _ of
    Product _ → ", "
    LeftJoin _ _ → " LEFT JOIN "

  showSource ∷ Source v → String
  showSource = case _ of
    Product t → showSQL t
    LeftJoin t e → showSQL t <> " ON (" <> showExpr e <> ")"

  showAliasedCol ∷ Tuple Alias (ExistsExpr v) → String
  showAliasedCol (Tuple alias ee) = runExists showExpr ee <> " AS " <> alias
