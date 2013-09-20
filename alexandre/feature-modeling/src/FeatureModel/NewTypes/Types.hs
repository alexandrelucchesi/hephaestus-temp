-- @author Alexandre Lucchesi
--
-- This module defines the basic types and type synonyms used by Hephaestus.

{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module FeatureModel.NewTypes.Types
( Cardinality(..)
, Constraints
, Feature(..)
, FeatureCardinality
--, FeatureExpression(..)
, FeatureId
, FeatureModel(..)
, FeatureName
, GroupCardinality
, SimplifiedFeature(..)
, alternative
, basic
, mandatory
, node
, optional
, or
)
where

--import qualified Data.Foldable as F
import Data.Generics

import qualified Data.List as L
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

data FeatureModel a = FeatureModel {
                        fModel     :: T.Tree (Feature a),
                        constraints :: [Constraints]
                    } deriving (Show)

-- Constructor exported for creating Feature Models.
-- It ensures that every feature created will be inside the GADT 'Feature'.
node :: (SimplifiedFeature a) => a -> T.Forest (Feature a) -> T.Tree (Feature a)
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

