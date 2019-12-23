module Test.Types where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Database.PostgreSQL (class FromSQLValue, class ToSQLValue)
import Foreign (ForeignError(..), F, readString, unsafeToForeign)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

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

readAccountTypeF ∷ String → F AccountType
readAccountTypeF =
  except <<< lmap (singleton <<< ForeignError) <<< readAccountType

instance readForeignAccountType ∷ ReadForeign AccountType where
  readImpl = readString >=> readAccountTypeF

instance writeForeignAccountType ∷ WriteForeign AccountType where
  writeImpl = printAccountType >>> write
