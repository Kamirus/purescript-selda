module Selda.Lit where

import Prelude

import Data.Exists (mkExists)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (class ToSQLValue)
import Selda.Aggr (class Coerce, unsafeFromCol)
import Selda.Col (Col(..))
import Selda.Expr (Expr(..), Literal(..), None(..))
import Selda.Inner (Inner)
import Selda.PG (litPG)
import Selda.PG.Class (BackendPGClass)
import Selda.SQLite3 (litSQLite3)
import Selda.SQLite3.Class (BackendSQLite3Class)
import Simple.JSON (class WriteForeign)
import Unsafe.Coerce (unsafeCoerce)

-- | Lift a value `a` to a column expression using `Lit s a` typeclass.
lit
  :: forall col s a
   . Lit s a
  => Coerce col
  => a
  -> col s a
lit = unsafeFromCol <<< litImpl

class Lit :: forall k. k -> Type -> Constraint
class Lit s a where
  litImpl :: a -> Col s a

instance litBoolean :: Lit b Boolean where
  litImpl x = Col $ ELit $ LBoolean x identity

else instance litString :: Lit b String where
  litImpl x = Col $ ELit $ LString x identity

else instance litInt :: Lit b Int where
  litImpl x = Col $ ELit $ LInt x identity

else instance litMaybe :: Lit b a => Lit b (Maybe a) where
  litImpl = case _ of
    Nothing -> Col $ ELit $ LNull $ mkExists $ None identity
    Just l -> liftJust $ litImpl l
      where
      liftJust :: Col b a -> Col b (Maybe a)
      liftJust = unsafeCoerce

else instance ilitPG :: ToSQLValue a => Lit BackendPGClass a where
  litImpl = litPG

else instance ilitSQLite3 :: WriteForeign a => Lit BackendSQLite3Class a where
  litImpl = litSQLite3

else instance litInner :: Lit s a => Lit (Inner s) a where
  litImpl a = case (litImpl a :: Col s a) of Col e -> Col e
