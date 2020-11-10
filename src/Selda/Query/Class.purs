module Selda.Query.Class where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Data.Either (Either, either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Selda.Col (Col)
import Selda.Table (Table)
import Selda.Query.ShowStatement (class GenericShowInsert, genericShowInsert)
import Selda.Query.Type (FullQuery)
import Selda.Query.Utils (RecordToArrayForeign(..))
import Type.Proxy (Proxy)

class Monad m <= GenericQuery b m i o | i → o, b → m where
  genericQuery
    ∷ Proxy b
    → FullQuery b { | i }
    → m (Array { | o })

class Monad m <= GenericInsert b m t r | t → r, b → m where
  genericInsert
    ∷ Proxy b
    → Table t
    → Array { | r }
    → m Unit

class Monad m <= GenericDelete b m t r | t → r, b → m where
  genericDelete
    ∷ Proxy b
    → Table t
    → ({ | r } → Col b Boolean)
    → m Unit

class Monad m <= GenericUpdate b m t r | t → r, b → m where
  genericUpdate
    ∷ Proxy b 
    → Table t
    → ({ | r } → Col b Boolean)
    → ({ | r } → { | r })
    → m Unit

-- | parametrized implementation of `genericInsert`
genericInsert_
  ∷ ∀ t r a b
  . GenericShowInsert t r
  ⇒ HFoldl (RecordToArrayForeign b) (Array Foreign) { | r } (Array Foreign)
  ⇒ { ph ∷ String, exec ∷ String → Array Foreign → a }
  → Proxy b
  → Table t
  → Array { | r }
  → a
genericInsert_ { ph, exec } b table rs = do
  let
    q = genericShowInsert { ph } table rs
    l = rs >>= hfoldl (RecordToArrayForeign b) ([] ∷ Array Foreign)
  exec q l

hoistSeldaWith
  ∷ ∀ r e' e m r'
  . MonadThrow e' m
  ⇒ MonadAsk r' m
  ⇒ MonadAff m
  ⇒ (e → e')
  → (r' → r)
  → ExceptT e (ReaderT r Aff) ~> m
hoistSeldaWith fe fr m = do
  conn ← asks fr
  runReaderT (runExceptT m) conn # liftAff
    >>= either (throwError <<< fe) pure

class
  ( MonadAff m
  , MonadError e m
  , MonadReader r m
  ) <= MonadSelda m e r | m → e r

instance monadSeldaInstance
  ∷ ( MonadAff m
    , MonadError e m
    , MonadReader r m
    )
  ⇒ MonadSelda m e r

runSelda
  ∷ ∀ a e r
  . r
  → ExceptT e (ReaderT r Aff) a
  → Aff (Either e a)
runSelda conn m = runReaderT (runExceptT m) conn
