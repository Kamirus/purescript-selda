module BottomUp where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Row0(..), PoolConfiguration, execute, newPool, query, withConnection)
import Database.PostgreSQL as PG
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)

type Table = { name ∷ String , alias ∷ String }

t1 ∷ Table
t1 = { name: "persons", alias: "p1" }

t2 ∷ Table
t2 = { name: "persons", alias: "p2" }

newtype Col a = Col
  { table ∷ Table
  , name ∷ String
  }

s1 = 
  { sources: [ t1, t1 { alias = "p2" } ]
  , restricts: []
  , res: 
      { id1: Col { table: t1, name: "id" } ∷ Col Int
      , name1: Col { table: t1, name: "name" } ∷ Col String
      , id2: Col { table: t2, name: "id" } ∷ Col Int
      , name2: Col { table: t2, name: "name" } ∷ Col String
      }
  }

q1 = proc s1

data ShowNames = ShowNames

instance showNames
    ∷ IsSymbol sym
    ⇒ FoldingWithIndex ShowNames (SProxy sym) (Array String) (Col a) (Array String)
  where
  foldingWithIndex ShowNames prop acc (Col { table, name }) = 
    acc <> [table.alias <> "." <> name <> " " <> reflectSymbol prop]

recordNames
  ∷ ∀ a
  . HFoldlWithIndex ShowNames (Array String) a (Array String)
  ⇒ a
  → (Array String)
recordNames r = hfoldlWithIndex ShowNames ([] ∷ Array String) r 

pg ∷ Aff Unit
pg = do
  pool ← newPool dbconfig
  withConnection pool \conn → do
    -- rows ← query conn (PG.Query """
    --   select id, name from persons;
    -- """) Row0
    liftEffect $ log q1
    rows ← query conn (PG.Query q1) Row0
    liftEffect $ for_ rows \((id1 ∷ Int) /\ (id2 ∷ Int) /\ (name1 ∷ String) /\ (name2 ∷ String)) → do
      log $ name1 <> " " <> show id1 <> " " <> name2 <> " " <> show id2

dbconfig ∷ PoolConfiguration
dbconfig =
  { database: "selda"
  , host: Just $ "127.0.0.1"
  , idleTimeoutMillis: Just $ 1000
  , max: Just $ 10
  , password: Just $ "qwerty"
  , port: Just $ 5432
  , user: Just $ "init"
  }

main ∷ Effect Unit
main = launchAff_ $ do
  pg
  -- let (names ∷ Array String) = recordNames { a: "xd", b: 1 } in
  -- logShow names
  

-- recordNames r = hfoldlWithIndex f x r

proc s = "select " <> cols <> " from " <> tables <> ";"
  where
  cols = joinWith ", " $ recordNames s.res
  tables = joinWith ", " $ map (\t → t.name <> " " <> t.alias) s.sources



{-

-}


