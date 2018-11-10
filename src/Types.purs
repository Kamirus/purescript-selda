module Types where
  
import Prelude

import Control.Monad.State (State, get, put)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Expr (Col(..), Expr)
import Prim.Row as R
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record as Record
import Type.Row (RLProxy(..))

