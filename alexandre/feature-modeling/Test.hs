-----------------------------------------------
-- Test file
-----------------------------------------------
import Control.Applicative ((<$>))

import FeatureModel.Types as Old hiding (eval)
import FeatureModel.NewTypes.Types as New hiding (Feature)
import FeatureModel.Logic as New hiding (eval)

import OldMain as Old
import NewMain as New

import FeatureModel.FMTypeChecker as Old
import FeatureModel.NewFMTypeChecker as New

import TestSetup
-----------------------------------------------
-- ASSERTS
-----------------------------------------------
tests =
    [
        ("assertTrue_ftree1", assertTrue_ftree1),
        ("assertTrue_checkFeatureTree", assertTrue_checkFeatureTree),
        ("assertTrue_checkFeatureTree'", assertTrue_checkFeatureTree),
        ("assertTrue_checkFeatureTree''", assertTrue_checkFeatureTree),
        ("assertTrue_missingAlternative", assertTrue_missingAlternatives),
        ("assertTrue_checkConstraintImposingAlternative", assertTrue_checkConstraintImposingAlternative),
        ("assertFalse_dummy", True),
        ("assertFalse_dummy", False)
    ]

-- []     means no error occurred.
-- (x:xs) contains the errors' names.
runAllTests = let res = foldr eval [] tests
                  in case res of
                          [] -> putStrLn "All tests were executed SUCCESSFULLY!"
                          xs -> putStrLn $ "The following tests FAILED: " ++ show xs ++ "."

eval :: (String, Bool) -> [String] -> [String]
eval (name, v) s =
    let prefix = takeWhile (/= '_')
        suffix = dropWhile (\x -> x /= 'F' && x /= 'T')
        expectedRes = read $ (prefix . suffix) name :: Bool
        in if v == expectedRes
              then s
              else name : s

assertTrue_ftree1 = Old.featureToPropositionalLogic ftree1_old == New.featureToPropositionalLogic ftree1_new
assertTrue_treeMobilePhone = Old.featureToPropositionalLogic treeMobilePhone_old == New.featureToPropositionalLogic treeMobilePhone_new
assertTrue_fmMobilePhone = Old.fmToPropositionalLogic fmMobilePhone_old == New.fmToPropositionalLogic fmMobilePhone_new

-- Helpers
assertIO :: (Monad m, Eq a) => m a -> m a -> m Bool
assertIO action1 action2 = do
    x <- action1
    y <- action2
    return (x == y)

-- Main Functions
assertTrue_execSummary  = assertIO (Old.execSummary fmMobilePhone_old) (New.execSummary fmMobilePhone_new)
assertFalse_execSummary = not <$> assertIO (Old.execSummary fmTree1_old) (New.execSummary fmMobilePhone_new)
    where
        fmTree1_old = Old.FeatureModel ftree1_old []
-- The test below is "dummy". Check the files "mobile_*.cnf" generated, they must be equal (tip: use "vimdiff" tool).
-- NOTE: Maybe the function exec2Fm2Cnf would be more testable if it took a Handler instead of a String as its first parameter.
assertTrue_execFm2Cnf = assertIO (Old.execFm2Cnf "mobile_old.cnf" fmMobilePhone_old) (New.execFm2Cnf "mobile_new.cnf" fmMobilePhone_new)

-- FMTypeChecker Functions
assertTrue_checkFeatureTree   = Old.checkFeatureTree ftree1_old == New.checkFeatureTree ftree1_new
assertTrue_checkFeatureTree'  = Old.checkFeatureTree (Old.fmTree fmMobilePhone_old) == New.checkFeatureTree (New.fmTree fmMobilePhone_new)
assertTrue_checkFeatureTree'' = Old.checkFeatureTree ftree1_old == New.checkFeatureTree (New.fmTree fmMobilePhone_new)
assertTrue_missingAlternatives =
    let new = New.missingAlternatives fmTree1_new
        old = Old.missingAlternatives fmTree1_old
        in if length new == length old
              then and $ zipWith equals new old
              else False
assertTrue_checkConstraintImposingAlternative = False
assertTrue_checkConstraintImposingOptional = False
assertTrue_superimposedOptional = False
assertTrue_checkDeadFeatures = False
assertTrue_findBadSmells = False

