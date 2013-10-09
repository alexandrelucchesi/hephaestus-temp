-- @author Alexandre Lucchesi
--
-- This module defines the basic types and type synonyms used by Hephaestus.

{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module FeatureModel.NewTypes.Types
( Cardinality(..)
--, CheckerResult
, Constraints
, Feature(..)
, FeatureCardinality
, FeatureConfiguration(..)
--, FeatureExpression(..)
, FeatureId
, FeatureModel(..)
, FeatureName
, FeatureTree
, FeatureForest
, GroupCardinality
, SimplifiedFeature(..)
, alternative
, basic
, children
, flatten
, fnode
, foldFTree
, mandatory
, node
, optional
, or
, getVars
, getClauses
, expToLiterals
, subtrees
) where

--import qualified Data.Foldable as F
import Data.Generics

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Tree as T

--import FeatureModel.Logic
import FeatureModel.Types (FeatureExpression(..))
import Funsat.Types

import Prelude hiding (or)

------------------------------------------------
-- Simplified Feature
------------------------------------------------
type FeatureId = String
type FeatureName = String
type FeatureCardinality = Cardinality
type GroupCardinality = Cardinality

class (Show a, Eq a) => SimplifiedFeature a where
    fId          :: a -> FeatureId
    fName        :: a -> FeatureName
    fCardinality :: a -> FeatureCardinality
    gCardinality :: a -> GroupCardinality

-- GADT (holder).
data Feature a where
    Feature :: (SimplifiedFeature a) => a -> Feature a

-- Syntax for deriving instances when using GADTs.
-- *Hence the "StandaloneDeriving" pragma.
deriving instance Eq (Feature a)
--deriving instance Show (Feature a)
-- Made this temporarily for testing purposes.
instance Show (Feature a) where
    show (Feature x) = show x

-- Kind of fmap.

instance SimplifiedFeature (Feature a) where
    fId          (Feature f) = fId f
    fName        (Feature f) = fName f
    fCardinality (Feature f) = fCardinality f
    gCardinality (Feature f) = gCardinality f

------------------------------------------------
-- Cardinality (Generalizes Feature/Group Types)
------------------------------------------------

-- There's a good discussion ADTs x Tuples in
-- the book Real World Haskell.
-- I consider ADT better in this case.
data Cardinality = Cardinality {
    min :: Int,
    max :: Int
    } deriving (Eq, Show)

-- Feature types
optional  = Cardinality 0 1
mandatory = Cardinality 1 1

-- Group types
basic       = Cardinality 0 0
alternative = Cardinality 1 1
or          = Cardinality 1 -- Expects 'max' to be informed.

------------------------------------------------
-- Feature Model
------------------------------------------------
type Constraints = FeatureExpression
type FeatureTree   a = T.Tree (Feature a)
type FeatureForest a = [FeatureTree a]

data FeatureModel a = FeatureModel {
    fmTree        :: FeatureTree a,
    fmConstraints :: [Constraints]
    } deriving (Show)

-- Constructor exported for creating Feature Models.
-- It ensures that every feature created will be inside the GADT 'Feature'.
node :: (SimplifiedFeature a) => a -> FeatureForest a -> FeatureTree a
node = T.Node . Feature

------------------------------------------------
-- Constraints/Feature Expressions
------------------------------------------------
-- We're importing this data type from FeatureModel.Types.
-- NOTE: The definition below solves DEPRECATION WARNINGS.
--data FeatureExpression = ConstantExpression Bool
--                       | FeatureRef FeatureId
--                       | Not FeatureExpression
--                       | And FeatureExpression FeatureExpression
--                       | Or FeatureExpression FeatureExpression
--                       deriving (Eq, Typeable)
--
--instance Show FeatureExpression where
--  show (FeatureRef id) = show id
--  show (And exp1 exp2) = "And (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
--  show (Or exp1 exp2) = "Or (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
--  show (Not exp1) = "Not (" ++ (show exp1)  ++ ")"
--  show (ConstantExpression True) = "True"
--  show (ConstantExpression False) = "False"
--
--deriving instance Data FeatureExpression

newtype FeatureConfiguration a = FeatureConfiguration {
    fcTree :: FeatureTree a
    } deriving (Show, Typeable)

-- TODO: Add the following instance declaration.
--deriving instance Data (FeatureConfiguration a)


------------------------------------------------
-- Functions
------------------------------------------------
fnode    = T.rootLabel
children = \xs -> map fnode (T.subForest xs)
-- NOTE: "subtrees" is "flatten" in the old version.
flatten  = T.flatten
subtrees :: T.Tree a -> [T.Tree a]
subtrees t = grab t []
    where
    grab st@(T.Node _ ts) xs = st : foldr grab xs ts

-- TODO: Is it possible to change this function for foldr or foldMap (Data.Foldable)?
foldFTree :: (b -> b -> b) -> (FeatureTree a -> b) -> (FeatureTree a -> b) -> b -> FeatureTree a -> b
foldFTree f1 f2 f3 f4 (T.Node f [])  = f2 (T.Node f [])
foldFTree f1 f2 f3 f4 (T.Node f fs) = f1 (f3 (T.Node f fs)) (foldr (f1) f4 [foldFTree f1 f2 f3 f4 x | x <- fs])

getVars :: FeatureExpression -> [FeatureExpression]
getVars (And exp1 exp2)  = L.nub ((getVars exp1) ++ (getVars exp2))
getVars (Or  exp1 exp2)  = L.nub ((getVars exp1) ++ (getVars exp2))
getVars (Not exp1)       =  getVars exp1
getVars (FeatureRef f)   = [(FeatureRef f)]
getVars otherwise        = []

getClauses :: FeatureExpression -> [FeatureExpression]
getClauses (And exp1 exp2) = (getClauses exp1) ++ (getClauses exp2)
getClauses (Or  exp1 exp2) = [Or exp1 exp2]
getClauses (Not exp1)      = [Not exp1]
getClauses (FeatureRef f)  = [FeatureRef f]
getClauses otherwhise      = []

expToLiterals :: [FeatureExpression] -> FeatureExpression -> Clause
expToLiterals fs e = map intToLiteral (expToLiterals' fs e)
    where
        expToLiterals' fs (Or  exp1 exp2) = (expToLiterals' fs exp1) ++ (expToLiterals' fs exp2)
        expToLiterals' fs (Not exp1)      = map (*(-1)) (expToLiterals' fs exp1)
        expToLiterals' fs (FeatureRef f)  =
            if M.isJust (L.elemIndex (FeatureRef f) fs)
               then [M.fromJust(L.elemIndex (FeatureRef f) fs) + 1]
               else []
        expToLiterals' fs otherwise = []
        intToLiteral x = L { unLit = x }

