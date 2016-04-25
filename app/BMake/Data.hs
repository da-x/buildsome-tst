{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module BMake.Data
    ( MetaVar(..)
    , MetaVarModifier(..)
    , Expr3(..) -- ToDo: rename
    ) where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           GHC.Generics
import           Data.ByteString.Lazy (ByteString)
--------------------------------------------------------------------------------

data MetaVar
  = FirstOutput
  | FirstInput
  | AllInputs
  | AllOOInputs
  | Stem
  deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVar where
instance NFData MetaVar where
  rnf = genericRnf

data MetaVarModifier
  = NoMod
  | ModFile
  | ModDir
  deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVarModifier where
instance NFData MetaVarModifier where
  rnf = genericRnf

data Expr3 -- ToDo: rename
  = Expr3'Str ByteString
  | Expr3'Spaces
  | Expr3'VarSpecial MetaVar MetaVarModifier
  deriving (Eq, Ord, Show, Generic)
instance NFData Expr3

