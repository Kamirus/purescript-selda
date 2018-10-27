module Main where

import Prelude

import Control.Monad.State (State, get, put)
import Data.Symbol (class IsSymbol, SProxy(..))
import Heterogeneous.Mapping (class MappingWithIndex, hmap, hmapWithIndex, mappingWithIndex)
import Prim.Row as Row
import Record as Record

{- 
how to define table?
- names and types in # Type
- primary? autoinc? unique? optional?
- foreign key? 
-}

newtype Table ( r ∷ # Type ) = Table { name ∷ String , alias ∷ String }

-- newtype Query s a = Query (State GenState a)
-- derive newtype instance functorQuery ∷ Functor (Query s)
-- derive newtype instance applyQuery ∷ Apply (Query s)
-- derive newtype instance applicativeQuery ∷ Applicative (Query s)
-- derive newtype instance bindQuery ∷ Bind (Query s)
-- derive newtype instance monadQuery ∷ Monad (Query s)

people ∷ Table ( name ∷ String , age ∷ Int , id ∷ Int )
people = Table { name: "people" , alias: "people" }

-- newtype Col name alias a = Col 
--   { name ∷ SProxy name
--   , alias ∷ SProxy alias
--   }

-- eq' ∷ ∀ name alias a. IsSymbol name ⇒ Col name alias a → a → String
-- eq' 

-- func ∷ ∀ a sym. IsSymbol sym ⇒ SProxy sym → a → Col (SProxy sym) a
-- func _ a = Col a

newtype ZipProps a b = ZipProps (a → b)

instance zipProps
    ∷ IsSymbol sym
    ⇒ MappingWithIndex (ZipProps a b) (SProxy sym) a b
  where
  mappingWithIndex (ZipProps f) prop a = f a

type GenState = 
  { nameSupply ∷ Int
  -- , sources ∷ Array Select
  -- , nameScope ∷ Int
  }

-- data SqlSource
--   = 

-- renameAll
--   ∷ Table ( name ∷ String , id ∷ Int )
--   → State GenState { name ∷ Col "name" "name_1" String , id ∷ Col "id" "id_3" Int }
-- renameAll t = do
--   st ← get
--   let
--     name = Col { name: SProxy ∷ SProxy "name", alias: SProxy ∷ SProxy "name_1" }
--     id = Col { name: SProxy ∷ SProxy "id", alias: SProxy ∷ SProxy "id_3" }
--   pure { name , id }

newtype Col row name a = Col
  { table ∷ Table row
  , name ∷ SProxy name
  }

mkCol 
  ∷ ∀ row name a tail
  . Row.Cons name a tail row
  ⇒ Table row → SProxy name → Col row name a --{ table ∷ Table row , name ∷ SProxy name }
mkCol t name = Col { table: t, name }
-- aux = mkCol people (SProxy ∷ SProxy "name")

select 
  ∷ Table ( name ∷ String , id ∷ Int )
  → State GenState { name ∷ Col _ "name" String , id ∷ Col _ "id" Int }
select (Table t) = do
  st ← get
  put $ st { nameSupply = st.nameSupply + 1 }
  let
    t' = Table $ t { alias = t.name <> show st.nameSupply }
    name = mkCol t' $ SProxy ∷ SProxy "name"
    id = mkCol t' $ SProxy ∷ SProxy "id"
  pure $ { name, id }

-- foo = do
--   { name, age, id } ← select people
--   p2 ← select people
--   restrict $ id == p2.id
--   restrict $ age > 10
--   pure { name, id }
