module PG.Exp where

-- import Prelude

-- import Data.Newtype (class Newtype)
-- import Expr (class ExpOps, class ExpRepr)
-- import Prim.RowList (kind RowList)
-- import Types (showCol)

-- newtype EI a = EI String
-- derive instance newtypeEI ∷ Newtype (EI a) _

-- instance eiCol ∷ ExpRepr EI where
--   fromCol = EI <<< showCol
--   fromInt = EI <<< show
--   fromBoolean = EI <<< show
--   fromString s = EI $ "'" <> s <>"'"

-- instance eiOps ∷ ExpOps EI where
--   eqq (EI e1) (EI e2) = EI $ "(" <> e1 <> "=" <> e2 <> ")"
--   gt (EI e1) (EI e2) = EI $ "(" <> e1 <> ">" <> e2 <> ")"
