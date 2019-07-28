module Test.Types where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Database.PostgreSQL (class FromSQLValue, class ToSQLValue)
import Foreign (readString, unsafeToForeign)

data AccountType
  = Business
  | Personal
derive instance eqAccountType ∷ Eq AccountType
derive instance genericAccountType ∷ Generic AccountType _
instance showAccountType ∷ Show AccountType where
  show = genericShow

readAccountType ∷ String → Either String AccountType
readAccountType "business" = Right Business
readAccountType "personal" = Right Personal
readAccountType  other = Left $ "Incorrect account type: " <> other

printAccountType ∷ AccountType → String
printAccountType Business = "business"
printAccountType Personal = "personal"

instance fromSqlValueAccountType ∷ FromSQLValue AccountType where
  fromSQLValue = readAccountType <=< lmap show <<< runExcept <<< readString

instance toSQLValueProductType ∷ ToSQLValue AccountType where
  toSQLValue = printAccountType >>> unsafeToForeign

