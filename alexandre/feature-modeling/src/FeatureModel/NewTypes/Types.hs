-- @author Alexandre Lucchesi
--
-- This module defines the basic types and type synonyms used by Hephaestus.

{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable #-}

module FeatureModel.NewTypes.Types
( Cardinality(..)
, Constraints
, FeatureCardinality
--, FeatureExpression(..)
, FeatureId
, FeatureModel(..)
, FeatureName
, GroupCardinality
, SimplifiedFeature(..)
, alternative
, basic
, featureToPropositionalLogic
, fmToPropositionalLogic
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

-- The constant expressions for representing True and False.
expTrue = ConstantExpression True
expFalse = ConstantExpression False

-- Check if a expression is an implies expression.
isImpliesExpression :: FeatureExpression -> Bool
isImpliesExpression (Or (Not e1) (e2)) = True
isImpliesExpression otherwise = False

-- Syntatic sugars for building expressions.
(|=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 |=> e2 = Or (Not e1) e2

(<=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 <=> e2 = And (Or (Not e1) e2) (Or (Not e2) e1)

(/\) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 /\ e2 = And e1 e2

(\/) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 \/ e2 = Or e1 e2

foldAnd xs = simplifyExpression (foldr And (expTrue) xs)
foldOr xs  = simplifyExpression (foldr Or  (expFalse) xs)

ref :: Feature a -> FeatureExpression
ref f = FeatureRef (fId f)
--
-- Expression simplifications
simplifyExpression :: FeatureExpression -> FeatureExpression
simplifyExpression (And e1 e2) = simplifyAnd e1 e2
simplifyExpression (Or e1 e2)  = simplifyOr e1 e2
simplifyExpression (Not e)     = simplifyNot e
simplifyExpression (FeatureRef f)         = FeatureRef f
simplifyExpression (ConstantExpression b) = ConstantExpression b

simplifyAnd :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyAnd e1 e2
    | (e1 == expFalse) || (e2 == expFalse) = expFalse
    | e1 == expTrue = simplifyExpression e2
    | e2 == expTrue = simplifyExpression e1
    | otherwise = And (simplifyExpression e1) (simplifyExpression e2)

simplifyOr :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyOr e1 e2
    | (e1 == expTrue) || (e2 == expTrue) = expTrue
    | e1 == expFalse = simplifyExpression e2
    | e2 == expFalse = simplifyExpression e1
    | otherwise = Or (simplifyExpression e1) (simplifyExpression e2)

simplifyNot :: FeatureExpression -> FeatureExpression
simplifyNot e
    | e == expTrue = expFalse
    | e == expFalse = expTrue
    | otherwise = Not (simplifyExpression e)

-- TODO: Is it possible to change this function for foldr or foldMap (Data.Foldable)?
foldFTree :: (b -> b -> b) -> (T.Tree (Feature a) -> b) -> (T.Tree (Feature a) -> b) -> b -> T.Tree (Feature a) -> b
foldFTree f1 f2 f3 f4 (T.Node f [])  = f2 (T.Node f [])
foldFTree f1 f2 f3 f4 (T.Node f fs) = f1 (f3 (T.Node f fs)) (foldr (f1) f4 [foldFTree f1 f2 f3 f4 x | x <- fs])

fmToPropositionalLogic :: FeatureModel a -> [FeatureExpression]
fmToPropositionalLogic fm = rootProposition ++ ftPropositions ++ csPropositions
    where
        (T.Node f fs) = fModel fm
        ftPropositions  = foldFTree (++) (\(T.Node _ []) -> []) (featureToPropositionalLogic) [] (T.Node f fs)
        csPropositions = constraints fm
        rootProposition = [ref f]

featureToPropositionalLogic :: T.Tree (Feature a) -> [FeatureExpression]
featureToPropositionalLogic ftree =
    let
        f  = T.rootLabel ftree
        cs = map T.rootLabel (T.subForest ftree)
    in (
        case gCardinality f of
            (Cardinality 0 0)  -> [(ref f) |=> (ref c) | c <- cs, fCardinality c == mandatory]
            (Cardinality 1 1) -> [(ref f) |=> (foldOr [xor x (L.delete x cs) | x <- cs])]
            (Cardinality 1 _) -> [(ref f) |=> (foldOr [ref x | x <- cs])]
    ) ++ [(ref c) |=> (ref f) | c <- cs]

xor f [] = ref f
xor f xs = And (ref f) (foldAnd [Not (ref x) | x <- xs])

