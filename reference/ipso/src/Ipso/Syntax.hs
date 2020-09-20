{-# language TemplateHaskell #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Ipso.Syntax where

import Bound (Scope)
import Bound.Class (Bound(..))
import Bound.TH (makeBound)
import Control.Monad (ap)
import Data.Deriving (deriveEq1, deriveShow1, makeLiftEq)
import Data.Functor.Classes (Eq1(..))
import "text" Data.Text (Text)
import qualified "text-utf8" Data.Text as Utf8
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)

data Type a
  = TName Text
  | TVar a

  | TBool
  | TInt
  | TChar
  | TString

  | TArrow
  | TArray
  | TRecord
  | TVariant

  | TRowCons Text (Type a) (Type a)
  | TRowNil

  | TApp (Type a) (Type a)
  deriving (Functor, Foldable, Traversable)
makeBound ''Type
deriveEq1 ''Type
deriveShow1 ''Type

data TypeScheme
  = Forall
  { typeVariables :: Vector Text
  , typeBody :: Scope Int Type Void
  } deriving (Eq, Show)

data RecordPattern
  = RecordPattern
  { recordPatternFields :: Vector Text
  , recordPatternOpen :: Bool
  } deriving (Eq, Show)

data Pattern
  = PName Text
  | PRecord RecordPattern
  | PCtor Text (Maybe RecordPattern)
  | PUnnamed
  deriving (Eq, Show)

recordPatternNames :: RecordPattern -> Vector Text
recordPatternNames (RecordPattern fs _) = fs

patternNames :: Pattern -> Vector Text
patternNames p =
  case p of
    PName n -> Vector.singleton n
    PRecord rp -> recordPatternNames rp
    PCtor _ m_rp -> foldMap recordPatternNames m_rp
    PUnnamed -> mempty

data CaseBranch f a
  = CaseBranch Pattern (Scope Int f a)
  deriving (Functor, Foldable, Traversable)
$(return [])
instance (Monad f, Eq1 f) => Eq1 (CaseBranch f) where
  liftEq = $(makeLiftEq ''CaseBranch)
deriveShow1 ''CaseBranch

instance Bound CaseBranch where
  CaseBranch p e >>>= f = CaseBranch p (e >>>= f)

data StringPart f a
  = StringPart Utf8.Text
  | ExprPart (f a)
  deriving (Functor, Foldable, Traversable)
$(return [])
instance (Monad f, Eq1 f) => Eq1 (StringPart f) where
  liftEq = $(makeLiftEq ''StringPart)
deriveShow1 ''StringPart

instance Bound StringPart where
  sp >>>= f =
    case sp of
      StringPart s -> StringPart s
      ExprPart e -> ExprPart (e >>= f)

data Expr a
  = Name Text
  | Var a

  | BTrue
  | BFalse
  | IfThenElse (Expr a) (Expr a) (Expr a)

  | Int Int

  | Char Char

  | Array (Vector (Expr a))

  | Lam (Vector Pattern) (Scope Int Expr a)
  | App (Expr a) (Expr a)

  | String (Vector (StringPart Expr a))

  | Record (Vector (Text, Maybe (Expr a)))
  | Project (Expr a) Text

  | Ctor Text
  | Case (Expr a) (Vector (CaseBranch Expr a))
  deriving (Functor, Foldable, Traversable)
instance Applicative Expr where; pure = return; (<*>) = ap
instance Monad Expr where
  return = Var
  expr >>= f =
    case expr of
      Name n -> Name n
      Var a -> f a

      BTrue -> BTrue
      BFalse -> BFalse
      IfThenElse a b c -> IfThenElse (a >>= f) (b >>= f) (c >>= f)

      Int n -> Int n

      Char c -> Char c

      Array es -> Array ((>>= f) <$> es)

      Lam ps b -> Lam ps (b >>>= f)
      App a b -> App (a >>= f) (b >>= f)

      String s -> String ((>>>= f) <$> s)

      Record es -> Record $ (fmap.fmap.fmap) (>>= f) es
      Project a b -> Project (a >>= f) b

      Ctor c -> Ctor c
      Case a bs -> Case (a >>= f) ((>>>= f) <$> bs)
deriveEq1 ''Expr
deriveShow1 ''Expr

newtype ModuleName = ModuleName (Vector Text)
  deriving (Eq, Show)

data Module
  = Module
  { moduleName :: ModuleName
  , moduleContents :: Vector Decl
  } deriving (Eq, Show)

data Decl
  = Binding
  { bindingName :: Text
  , bindingType :: Maybe TypeScheme
  , bindingArgs :: Vector Pattern
  , bindingBody :: Scope Int Expr Void
  }
  | TypeAlias
  { typeAliasName :: Text
  , typeAliasArgs :: Vector Text
  , typeAliasBody :: Scope Int Type Void
  }
  | Import Import
  deriving (Eq, Show)

data Import
  = Basic { importModule :: ModuleName }
  | Renamed { importModule :: ModuleName, importName :: Text }
  | Selective { importModule :: ModuleName, importSelection :: ImportSelection }
  deriving (Eq, Show)

data ImportSelection
  = All
  | Some (Vector Text)
  deriving (Eq, Show)
