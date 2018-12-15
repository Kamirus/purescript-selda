module Selda.Query.Type where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Exists (Exists)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim.RowList (kind RowList)
import Selda.Expr (Expr)
import Selda.Table (AliasedTable, Alias)

-- table or subquery, each with alias
data SQL
  = FromTable AliasedTable
  | SubQuery Alias GenState

-- describes elements which appear after FROM in generated sql
-- `Product`: produced using `select` function, generates cartesian product
-- `LeftJoin` produces LEFT JOIN <SQL> on (<Expr>)
-- Current repr requires Product to be the first Source in sources
data Source
  = Product SQL
  | LeftJoin SQL (Expr Boolean)

-- main state
-- FROM components in `sources`
-- WHERE components in `restricts`
-- SELECT components in `cols`, list of `Expr a`, where type `a` is irrelevant
-- `nextId` provides fresh identifiers
type GenState = 
  { sources ∷ Array Source
  , restricts ∷ Array (Expr Boolean)
  , nextId ∷ Int
  , cols ∷ Array (Tuple Alias (Exists Expr))
  , aggr ∷ Array (Exists Expr)
  }

-- type IGenState = { head ∷ SQL, st ∷ GenState }

newtype Query s a = Query (State GenState a)
derive newtype instance functorQuery ∷ Functor (Query s)
derive newtype instance applyQuery ∷ Apply (Query s)
derive newtype instance applicativeQuery ∷ Applicative (Query s)
derive newtype instance bindQuery ∷ Bind (Query s)
derive newtype instance monadQuery ∷ Monad (Query s)

newtype IQuery s a = IQuery (Query s a)
derive instance newtypeIQuery ∷ Newtype (IQuery s a) _
derive newtype instance functorIQuery ∷ Functor (IQuery s)
derive newtype instance applyIQuery ∷ Apply (IQuery s)
derive newtype instance applicativeIQuery ∷ Applicative (IQuery s)
derive newtype instance bindIQuery ∷ Bind (IQuery s)
derive newtype instance monadIQuery ∷ Monad (IQuery s)

initState ∷ GenState
initState = 
  { sources: []
  , restricts: []
  , nextId: 0
  , cols: []
  , aggr: []
  }

freshId ∷ ∀ s. Query s Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery ∷ ∀ a s. Query s a → Tuple a GenState
runQuery (Query st) = runState st initState
