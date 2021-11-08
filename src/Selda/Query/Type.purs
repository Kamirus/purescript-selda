module Selda.Query.Type where

import Prelude

import Control.Monad.State (class MonadState, State, runState)
import Control.Monad.State as State
import Data.Exists (Exists)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple)
import Selda.Expr (Expr)
import Selda.Table (AliasedTable, Alias)
import Unsafe.Coerce (unsafeCoerce)

-- table or subquery, each with alias
data SQL
  = FromTable AliasedTable
  | SubQuery Alias GenState

-- describes elements which appear after FROM in generated sql
data Source
  = From SQL
  | CrossJoin Source SQL
  | JoinOn JoinType Source SQL (Expr Boolean)
  | Combination QBinOp GenState GenState Alias

data JoinType
  = LeftJoin
  | InnerJoin

data QBinOp
  = Union
  | UnionAll
  | Intersect
  | Except

-- main state
-- FROM+JOIN[S] components in `source`
-- WHERE components in `restricts`
-- SELECT components in `cols`, list of `Expr a`, where type `a` is irrelevant
-- `nextId` provides fresh identifiers
type GenState_ =
  { source :: Source
  , restricts :: Array (Expr Boolean)
  , havings :: Array (Expr Boolean)
  , nextId :: Int
  , cols :: Array (Tuple Alias (Exists Expr))
  , aggr :: Array (Exists Expr)
  , order :: Array (Tuple Order (Exists Expr))
  , limit :: Maybe Int
  , offset :: Maybe Int
  , distinct :: Boolean
  }

newtype GenState = GenState GenState_

derive instance newtypeGenState :: Newtype GenState _

-- | Represents an intermediate query state.
-- | Before being wrapped with FullQuery this state represents SQL query without
-- | FROM component, but having every other including JOIN[s]
newtype Query :: forall k. k -> Type -> Type
newtype Query s a = Query (State GenState a)

derive instance newtypeQuery :: Newtype (Query s a) _
derive newtype instance functorQuery :: Functor (Query s)
derive newtype instance applyQuery :: Apply (Query s)
derive newtype instance applicativeQuery :: Applicative (Query s)
derive newtype instance bindQuery :: Bind (Query s)
derive newtype instance monadQuery :: Monad (Query s)
derive newtype instance stateQuery :: MonadState GenState (Query s)

-- | wrapper for query that is ready for SQL generation
newtype FullQuery :: forall k. k -> Type -> Type
newtype FullQuery s a = FullQuery (Query s a)

derive instance newtypeFullQuery :: Newtype (FullQuery s a) _
derive newtype instance functorFullQuery :: Functor (FullQuery s)
derive newtype instance applyFullQuery :: Apply (FullQuery s)
derive newtype instance applicativeFullQuery :: Applicative (FullQuery s)
derive newtype instance bindFullQuery :: Bind (FullQuery s)
derive newtype instance monadFullQuery :: Monad (FullQuery s)

data Order = Asc | Desc

initState :: GenState
initState = GenState
  { source: (unsafeCoerce unit :: Source)
  , restricts: []
  , havings: []
  , nextId: 0
  , cols: []
  , aggr: []
  , order: []
  , limit: Nothing
  , offset: Nothing
  , distinct: false
  }

get :: forall s. Query s GenState_
get = unwrap <$> State.get

put :: forall s. GenState_ -> Query s Unit
put = State.put <<< wrap

modify_ :: forall s. (GenState_ -> GenState_) -> Query s Unit
modify_ f = do
  st <- get
  put $ f st

freshId :: forall s. Query s Int
freshId = do
  st <- get
  put $ st { nextId = st.nextId + 1 }
  pure st.nextId

runQuery :: forall a s. Query s a -> Tuple a GenState
runQuery (Query st) = runState st initState

runFullQuery :: forall a s. FullQuery s a -> Tuple a GenState
runFullQuery = unwrap >>> runQuery
