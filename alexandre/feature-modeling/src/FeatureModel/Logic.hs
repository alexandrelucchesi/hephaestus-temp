module FeatureModel.Logic
( dimacsFormat
, eval
, featureToPropositionalLogic
, fmToPropositionalLogic
, fmToTseitinEncode
) where

import Data.Generics

import FeatureModel.NewTypes.Types

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tree as T

import FeatureModel.Types (FeatureExpression(..))
import Funsat.Types

import Prelude hiding (or)

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
        (T.Node f fs) = fmTree fm
        ftPropositions  = foldFTree (++) (\(T.Node _ []) -> []) (featureToPropositionalLogic) [] (T.Node f fs)
        csPropositions = fmConstraints fm
        rootProposition = [ref f]

featureToPropositionalLogic :: T.Tree (Feature a) -> [FeatureExpression]
featureToPropositionalLogic ftree =
    let f  = T.rootLabel ftree
        cs = map T.rootLabel (T.subForest ftree)
    in (
        case gCardinality f of
             (Cardinality 0 0)  -> [(ref f) |=> (ref c) | c <- cs, fCardinality c == mandatory]
             (Cardinality 1 1) -> [(ref f) |=> (foldOr [xor x (L.delete x cs) | x <- cs])]
             (Cardinality 1 _) -> [(ref f) |=> (foldOr [ref x | x <- cs])]
    ) ++ [(ref c) |=> (ref f) | c <- cs]

xor f [] = ref f
xor f xs = And (ref f) (foldAnd [Not (ref x) | x <- xs])

fmToCNFExpression :: FeatureModel a -> FeatureExpression
fmToCNFExpression fm =
    let fmExpressions = fmToPropositionalLogic fm
        in toCNFExpression (foldAnd fmExpressions)

fmToTseitinEncode :: FeatureModel a -> FeatureExpression
fmToTseitinEncode fm =
    let fmExpressions = fmToPropositionalLogic fm
        in toTseitinEncode (foldAnd fmExpressions)

toTseitinEncode :: FeatureExpression -> FeatureExpression
toTseitinEncode (Or e1 e2) =
    let a1 = newRef [1]
        a2 = newRef [2]
        in foldAnd ([Or a1 a2] ++ (toTseitinEncode' [1] e1) ++ (toTseitinEncode' [2] e2))

toTseitinEncode (And e1 e2) =
    let a1 = newRef [1]
        a2 = newRef [2]
        in foldAnd ([And a1 a2] ++ (toTseitinEncode' [1] e1) ++ (toTseitinEncode' [2] e2))

toTseitinEncode (Not e1) =
    let a1 = newRef [1]
        in foldAnd( [Not a1] ++ (toTseitinEncode' [1] e1))

toTseitinEncode e = e

-- toTseitinEncode' _  (FeatureRef e) = []

toTseitinEncode' gs (Or e1 e2) =
    let gl = gs ++ [1]
        gr = gs ++ [2]
        w  = newRef gs
        w1 = newRef gl
        w2 = newRef gr
        in  [And (Or (Not w) (Or w1 w2) ) (And (Or w (Not w1)) (Or w (Not w2)))] ++
            (toTseitinEncode' gl e1) ++
                (toTseitinEncode' gr e2)

toTseitinEncode' gs (And e1 e2) =
    let gl = gs ++ [1]
        gr = gs ++ [2]
        w  = newRef gs
        w1 = newRef gl
        w2 = newRef gr
        in [And (Or (Not w) w1) (And (Or (Not w) w2) (Or w (Or (Not w1) (Not w2))))] ++
            (toTseitinEncode' gl e1) ++
                (toTseitinEncode' gr e2)

toTseitinEncode' gs (Not e1) =
    let gl = gs ++ [1]
        w  = newRef gs
        w1 = newRef gl
        in [And (Or (Not w) (Not w1)) (Or w w1) ] ++ (toTseitinEncode' gl e1)

toTseitinEncode' gs otherwise = []

toCNFExpression :: FeatureExpression -> FeatureExpression
toCNFExpression (And e1 e2)    = And (toCNFExpression e1) (toCNFExpression e2)
toCNFExpression (Or e1 e2)     = distributeAndOverOr e1 e2
toCNFExpression (Not e1)       = moveNotInwards e1
toCNFExpression (FeatureRef f) = (FeatureRef f)

distributeAndOverOr :: FeatureExpression -> FeatureExpression -> FeatureExpression
distributeAndOverOr (And x y) e2 = And (toCNFExpression (Or x e2)) (toCNFExpression(Or y e2))
distributeAndOverOr e1 (And x y) = And (toCNFExpression(Or e1 x)) (toCNFExpression(Or e1 y))
distributeAndOverOr e1 e2 = distributeAndOverOr' a b
    where
        distributeAndOverOr' (And x y) e = toCNFExpression (Or (And x y) e)
        distributeAndOverOr' e (And x y) = toCNFExpression (Or e (And x y))
        distributeAndOverOr' x y = Or (toCNFExpression x) (toCNFExpression  y)
        a = toCNFExpression e1
        b = toCNFExpression e2

moveNotInwards :: FeatureExpression -> FeatureExpression
moveNotInwards (And x y) = Or  (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Or x y)  = And (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Not x)   = toCNFExpression x
moveNotInwards e         =  Not e

type Gate = Integer

newRef :: [Gate] -> FeatureExpression
newRef gs = FeatureRef (foldl (++) "g" [show g | g <- gs])

dimacsFormat :: FeatureExpression -> CNF
dimacsFormat exp =
    let vars = getVars exp
        cs = map (expToLiterals vars) (getClauses exp)
        in CNF {
            numVars = length vars,
            numClauses = length cs,
            clauses =  S.fromList cs
            }

eval :: FeatureConfiguration a -> FeatureExpression -> Bool
eval config (FeatureRef f) = elem f [fId x | x <- T.flatten (fcTree config)]
eval config (Not e) = not (eval config e)
eval config (And e1 e2) = (eval config e1) && (eval config e2)
eval config (Or e1 e2) = (eval config e1) || (eval config e2)
eval _ (ConstantExpression e) = e

