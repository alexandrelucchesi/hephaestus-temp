-- @author Alexandre Lucchesi
--
-- This module defines the basic types and type synonyms used by Hephaestus.

{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module FeatureModel.NewTypes.Types
( Cardinality(..)
, Constraints
, FeatureCardinality
, FeatureExpression
, FeatureId
, FeatureModel(..)
, FeatureName
, GroupCardinality
, node
, SimplifiedFeature(..)
)
where

import qualified Data.Tree as T

import Funsat.Types
import Data.Generics

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
deriving instance Show (Feature a)

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
                        contraints :: [Constraints]
                    } deriving (Eq, Show)

-- Constructor exported for creating Feature Models.
-- It ensures that every feature created will be inside the GADT 'Feature'.
node :: (SimplifiedFeature a) => a -> T.Forest (Feature a) -> T.Tree (Feature a)
node = T.Node . Feature

------------------------------------------------
-- Constraints
------------------------------------------------
data FeatureExpression = ConstantExpression Bool
                       | FeatureRef String
                       | Not FeatureExpression
                       | And FeatureExpression FeatureExpression
                       | Or FeatureExpression FeatureExpression
                       deriving (Eq, Show, Typeable)

deriving instance Data FeatureExpression

