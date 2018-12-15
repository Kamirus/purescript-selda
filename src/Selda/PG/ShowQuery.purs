module Selda.PG.ShowQuery where

import Prelude

import Data.Array (foldl, reverse)
import Data.Array as Array
import Data.Exists (Exists, runExists)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Selda.Expr (Expr, showExpr)
import Selda.Query.Type (GenState, SQL(..), Source(..))
import Selda.Table (Alias)

showState ∷ GenState → String
showState { cols, sources, restricts, aggr } = 
  showCols cols
    <> showSources sources
    <> showRestricts restricts
    <> showGrouping aggr

showCols ∷ Array (Tuple Alias (Exists Expr)) → String
showCols = case _ of
  [] → ""
  xs → "SELECT " <> (joinWith ", " $ map showAliasedCol xs)

showSources ∷ Array Source → String
showSources sources = case Array.uncons $ reverse sources of
  Nothing → ""
  Just { head, tail } → " FROM "
    <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource head) tail
  -- Just { head: h@(Product t), tail } →
  --   " FROM " <> foldl (\acc x → acc <> sepFor x <> showSource x) (showSource h) tail
  -- Just { head: LeftJoin t _, tail } →
  --   -- join on the first place, drop it and interpret as Product
  --   showSources $ Product t : tail

showRestricts ∷ Array (Expr Boolean) → String
showRestricts = case _ of
  [] → ""
  xs → " WHERE " <> (joinWith " AND " $ map (\e → "(" <> showExpr e <> ")") xs)

showGrouping ∷ Array (Exists Expr) → String
showGrouping = case _ of
  [] → ""
  xs → " GROUP BY " <> (joinWith ", " $ map (runExists showExpr) xs)

showSQL ∷ SQL → String
showSQL = case _ of
  FromTable t →
    t.name <> " " <> t.alias
  SubQuery alias state → 
    "(" <> showState state <> ") " <> alias

sepFor ∷ Source → String
sepFor = case _ of
  Product _ → ", "
  LeftJoin _ _ → " LEFT JOIN "

showSource ∷ Source → String
showSource = case _ of
  Product t → showSQL t
  LeftJoin t e → showSQL t <> " ON (" <> showExpr e <> ")"

showAliasedCol ∷ Tuple Alias (Exists Expr) → String
showAliasedCol (Tuple alias ee) = runExists showExpr ee <> " AS " <> alias
