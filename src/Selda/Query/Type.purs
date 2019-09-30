module Selda.Query.Type where

import Prelude

import Control.Monad.State (State, get, put, runState)
import Data.Exists (Exists)
import Data.Functor.Variant (VariantF)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim.RowList (kind RowList)
import Selda.Table (AliasedTable, Alias)

-- table or subquery, each with alias
data SQL v
  = FromTable AliasedTable
  | SubQuery Alias (GenState v)

-- describes elements which appear after FROM in generated sql
-- `Product`: produced using `select` function, generates cartesian product
-- `LeftJoin` produces LEFT JOIN <SQL> on (<Expr>)
-- Current repr requires Product to be the first Source in sources
data Source v
  = Product (SQL v)
  | LeftJoin (SQL v) (VariantF v Boolean)

-- main state
-- FROM+JOIN[S] components in `sources`
-- WHERE components in `restricts`
-- SELECT components in `cols`, list of `Expr a`, where type `a` is irrelevant
-- `nextId` provides fresh identifiers
type GenState v = 
  { sources ∷ Array (Source v)
  , restricts ∷ Array (VariantF v Boolean)
  , nextId ∷ Int
  , cols ∷ Array (Tuple Alias (Exists (VariantF v)))
  , aggr ∷ Array (Exists (VariantF v))
  , order ∷ Array (Tuple Order (Exists (VariantF v)))
  , limit ∷ Maybe Int
  }

-- | Represents an intermediate query state.
-- | Before being wrapped with FullQuery this state represents SQL query without
-- | FROM component, but having every other including JOIN[s]
newtype Query s v a = Query (State (GenState v) a)
derive newtype instance functorQuery ∷ Functor (Query s v)
derive newtype instance applyQuery ∷ Apply (Query s v)
derive newtype instance applicativeQuery ∷ Applicative (Query s v)
derive newtype instance bindQuery ∷ Bind (Query s v)
derive newtype instance monadQuery ∷ Monad (Query s v)

-- | wrapper for query that is ready for SQL generation
-- | This could be simple record `{ head ∷ SQL, st ∷ GenState }`
-- | where `st` is state from wrapped query
newtype FullQuery s v a = FullQuery (Query s v a)
derive instance newtypeFullQuery ∷ Newtype (FullQuery s v a) _
derive newtype instance functorFullQuery ∷ Functor (FullQuery s v)

data Order = Asc | Desc

initState ∷ ∀ v. GenState v
initState = 
  { sources: []
  , restricts: []
  , nextId: 0
  , cols: []
  , aggr: []
  , order: []
  , limit: Nothing
  }

freshId ∷ ∀ s v. Query s v Int
freshId = Query do
  st ← get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery ∷ ∀ a s v. Query s v a → Tuple a (GenState v)
runQuery (Query st) = runState st initState
