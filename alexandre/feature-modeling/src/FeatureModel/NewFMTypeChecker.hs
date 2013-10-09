module FeatureModel.NewFMTypeChecker where

import FeatureModel.Types (CheckerResult(..), ErrorMessage, FeatureExpression(..))
import FeatureModel.NewTypes.Types
import FeatureModel.Logic

import Funsat.Types
import Funsat.Solver
import Funsat.Resolution

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Tree (Tree(Node))

--data CheckerResult = Success
--                   | Fail { errorList :: [ErrorMessage] }
--                   deriving (Show,Eq)
--
--type ErrorMessage  = String

fmTypeChecker :: FeatureModel a -> CheckerResult
fmTypeChecker fm =
    case errors of
        [] -> Success
        xs -> Fail { errorList = errors }
    where
        errors = if e == [] then checkIfSatisfiable else []
        e = (checkFeatureTree (fmTree fm)) ++ (checkConstraints fm) ++ (checkDuplications fm)
        checkIfSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]

checkFeatureTree :: FeatureTree a -> [ErrorMessage]
checkFeatureTree ftree = foldFTree (++) (checkFeature') (checkFeature') [] ftree
    where
    checkFeature' ftree =
        if ((gCardinality (fnode ftree)) /= basic) && (length (children ftree) == 0)
           then [("Expecting at least one child for feature" ++ (fId (fnode ftree)))]
           else []

fmSATSolver :: FeatureModel a -> (Solution, Stats, Maybe ResolutionTrace)
fmSATSolver fm = solve1 (dimacsFormat (fmToCNFExpression fm))

isSatisfiable :: FeatureModel a -> Bool
isSatisfiable fm =
    let res = fmSATSolver fm
        in isSAT res

isSAT (x, y, z) =
    case x of
         Sat a -> True
         otherwise -> False

checkConstraints :: FeatureModel a -> [ErrorMessage]
checkConstraints fm =
    let cnames = [(c,n) | c <- fmConstraints fm, n <- expNames c]
        fnames = [fId f | f <- flatten (fmTree fm)]
        inames = [(c,n) | (c,n) <- cnames, notElem n fnames] --invalid references of the constraint
        in ["Invalid feature " ++ n  ++ " found in the constraint " ++ show (c) | (c,n) <- inames]

expNames :: FeatureExpression -> [String]
expNames (And e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Or  e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Not e1)       = expNames e1
expNames (FeatureRef e) = [e]
expNames otherwise      = []

checkDuplications :: FeatureModel a -> [ErrorMessage]
checkDuplications fm =
    let fnames = [fId f | f <- flatten (fmTree fm)]
        dupps  = [f | f <- fnames, length (filter (==f) fnames) > 1]
        in ["Feature " ++ f ++ " is duplicated." | f <- dupps]

missingAlternatives :: FeatureModel a -> [Feature a]
missingAlternatives fm = [ fnode f
                         | f <- subtrees (fmTree fm)
                         , gCardinality (fnode f) /= basic
                         , length (children f) < 2
                         ]

checkConstraintImposingAlternative :: FeatureModel a -> [(FeatureExpression, Feature a)]
checkConstraintImposingAlternative fm =
    let cs = [(c, constraintImposingAlternative c (fmTree fm)) | c <- fmConstraints fm]
        in [(c, fromJust f) | (c,f) <- cs, isJust f]
    where
    constraintImposingAlternative :: FeatureExpression -> Tree (Feature a) -> Maybe (Feature a)
    constraintImposingAlternative (FeatureRef r) ftree =
     let f = head [fnode x | x <- (subtrees ftree), (fId (fnode x)) == r]
     in if f `elem` (alternativeChildren ftree)
         then Just f
         else Nothing
    constraintImposingAlternative (Or (Not (FeatureRef r1)) (FeatureRef r2)) ftree =
        let
        f1 = head [fnode x | x <- (subtrees ftree), (fId (fnode x)) == r1]
        f2 = head [fnode x | x <- (subtrees ftree), (fId (fnode x)) == r2]
        in if (f1 `elem` (essentialFeatures ftree)) && (f2 `elem` alternativeChildren ftree)
              then Just f2
              else Nothing
    constraintImposingAlternative otherwise ftree = Nothing

checkConstraintImposingOptional :: FeatureExpression -> Tree (Feature a) -> Maybe (Feature a)
checkConstraintImposingOptional (FeatureRef r) ftree =
    let f = head [fnode x | x <- (subtrees ftree), (fId (fnode x)) == r]
        in if (f `elem` (orChildren ftree ++ optionalFeatures ftree))
              then Just f
              else Nothing

superimposedOptional :: FeatureModel a -> [Feature a]
superimposedOptional fm =
    let ftree = fmTree fm
        in
        [ f
        | f <- nub (optionalFeatures ftree ++ alternativeChildren ftree ++ orChildren ftree)
        , not (isSatisfiable (addConstraint fm (Not (ref f))))
        ]

addConstraint :: FeatureModel a -> FeatureExpression -> FeatureModel a
addConstraint fm exp = fm { fmConstraints = exp : cs}
    where
    cs = fmConstraints fm

checkDeadFeatures :: FeatureModel a -> [Feature a]
checkDeadFeatures fm = [ fnode f
                       | f <- subtrees (fmTree fm)
                       , isOptional (fnode f)
                       , not (isSatisfiable (addConstraint fm (ref (fnode f))))
                       ]

type BadSmell = String

findBadSmells :: FeatureModel a -> [BadSmell]
findBadSmells fm =
    if ((fmTypeChecker fm == Success) && isSatisfiable fm)
       then
       ["Expecting at least 2 children in feature " ++ show (f) | f <- missingAlternatives fm] ++
           ["Feature " ++ show f ++ " is a dead feature" | f <- checkDeadFeatures fm] ++
               ["Constraint " ++ show c ++ " impose alternative feature " ++ show f | (c,f) <- checkConstraintImposingAlternative fm]
               else
               ["The feature model is either incorrect or inconsistent (unsatisfiable)"]
