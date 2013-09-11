-- @author Alexandre Lucchesi
--
-- This module defines some handy functions.
module NewTypes.Util where

import FeatureModel.NewTypes.Types
import Data.Tree

-- Print tree node names.
printFM :: FeatureModel a -> IO ()
printFM (FeatureModel tree _) = (putStrLn . drawTree . (fmap fName)) tree

printWholeFM :: FeatureModel a -> IO ()
printWholeFM (FeatureModel tree contraints) = do
    putStrLn "================================="
    putStrLn "FEATURE TREE"
    putStrLn "================================="  
    putStrLn . drawTree . (fmap show) $ tree
    putStrLn "================================="
    putStrLn "CONSTRAINTS"
    putStrLn "================================="
    putStrLn $ show contraints

--toString :: T.Tree Feature -> T.Tree String
--toString = fmap fName

-- Some commands to run:
--
-- Another way of doing what printFM does:
-- (putStrLn . T.drawTree . toString) fmMobilePhone
--
-- Printing features instead of features' names:
-- (putStrLn . T.drawTree) $ fmap show fmMobilePhone
--
-- Modifying a tree to contain only dummy features:
-- let dummyTree = Z.modifyTree (fmap (\_ -> basicFeature "[HASKELL]" Mandatory Basic)) (Z.fromTree fmMobilePhone)
--
-- Printing the dummyTree
-- printFM $ Z.toTree dummyTree 

