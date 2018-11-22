{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Agda.Compiler.JS.Syntax where

import Data.Maybe ( catMaybes )
import Data.Foldable (foldMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Agda.Syntax.Common ( Nat )

-- An untyped lambda calculus with records,
-- and a special self-binder for recursive declarations

data Exp =
  Self |
  Local LocalId |
  Global GlobalId |
  Undefined |
  Null |
  String String |
  Char Char |
  Integer Integer |
  Double Double |
  Lambda Nat Exp |
  Object (Map MemberId Exp) |
  Array [(Comment, Exp)] |
  Apply Exp [Exp] |
  Lookup Exp MemberId |
  If Exp Exp Exp |
  BinOp Exp String Exp |
  PreOp String Exp |
  Const String |
  PlainJS String -- ^ Arbitrary JS code.
  deriving (Show, Eq)

-- Local identifiers are named by De Bruijn indices.
-- Global identifiers are named by string lists.
-- Object members are named by strings.

newtype LocalId = LocalId Nat
  deriving (Eq, Ord, Show)

newtype GlobalId = GlobalId [String]
  deriving (Eq, Ord, Show)

data MemberId
    = MemberId String
    | MemberIndex Int Comment
  deriving (Eq, Ord, Show)

newtype Comment = Comment String
  deriving (Show, Semigroup, Monoid)

instance Eq Comment where _ == _ = True
instance Ord Comment where compare _ _ = EQ

-- The top-level compilation unit is a module, which names
-- the GId of its exports, and a list of definitions

data Export = Export { expName :: [MemberId], defn :: Exp }
  deriving Show

data Module = Module
  { modName :: GlobalId
  , imports :: [GlobalId]
  , exports :: [Export]
  , postscript :: Maybe Exp
  }
  deriving Show

-- Note that modules are allowed to be recursive, via the Self expression,
-- which is bound to the exported module.

class Uses a where
  uses :: a -> Set (Maybe GlobalId, [MemberId])

  default uses :: (a ~ t b, Foldable t, Uses b) => a -> Set (Maybe GlobalId, [MemberId])
  uses = foldMap uses

instance Uses a => Uses [a]
instance Uses a => Uses (Map k a)

instance (Uses a, Uses b) => Uses (a, b) where
  uses (a, b) = uses a `Set.union` uses b

instance (Uses a, Uses b, Uses c) => Uses (a, b, c) where
  uses (a, b, c) = uses a `Set.union` uses b `Set.union` uses c

instance Uses Comment where
  uses _ = Set.empty

instance Uses Exp where
  uses (Lambda n e)   = uses e
  uses (Object o)     = uses o
  uses (Array es)     = uses es
  uses (Apply e es)   = uses (e, es)
  uses (Lookup e l)   = uses' e [l] where
      uses' Self         ls = Set.singleton (Nothing, ls)
      uses' (Global i)   ls = Set.singleton (Just i, ls)
      uses' (Lookup e l) ls = uses' e (l : ls)
      uses' e            ls = uses e
  uses (If e f g)     = uses (e, f, g)
  uses (BinOp e op f) = uses (e, f)
  uses (PreOp op e)   = uses e
  uses e              = Set.empty

instance Uses Export where
  uses (Export _ e) = uses e

instance Uses Module where
  uses (Module _ _ es _) = uses es

-- Top-level uses of the form exports.l1....lN.
globals :: Uses a => a -> Set GlobalId
globals = Set.fromList . catMaybes . map fst . Set.toList . uses

