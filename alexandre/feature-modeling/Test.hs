-----------------------------------------------
-- Test file
-----------------------------------------------
import FeatureModel.Types as Old
import FeatureModel.NewTypes.Types as New

import Data.List hiding (or)
import Prelude hiding (or)
import Data.Tree

-----------------------------------------------
-- ASSERTS
-----------------------------------------------
assertTrue_ftree1 = Old.featureToPropositionalLogic ftree1_old == New.featureToPropositionalLogic ftree1_new

-----------------------------------------------
-- OLD
-----------------------------------------------

-- SAMPLE 01 ----------------------------------
ftree1_old =
    Root (Feature "F1" "F1" Mandatory "F1" BasicFeature [])
    [
        Leaf (Feature "F2" "F2" Mandatory "F2" BasicFeature []),
        Leaf (Feature "F3" "F3" Mandatory "F3" BasicFeature [])
    ]

-----------------------------------------------
-- NEW
-----------------------------------------------

-- SAMPLE 01 ----------------------------------
ftree1_new =
    node (MockFeature "F1" "F1" mandatory basic)
    [
        node (MockFeature "F2" "F2" mandatory basic) [],
        node (MockFeature "F3" "F3" mandatory basic) []
    ]

-- SAMPLE 02 ----------------------------------
-- Features
newfMobilePhone = MockFeature "mobile-phone" "Mobile Phone" mandatory basic
newfCalls       = MockFeature "calls" "Calls" mandatory basic
newfGPS         = MockFeature "gps" "GPS" optional basic
newfScreen      = MockFeature "screen" "Screen" mandatory alternative
newfbasic       = MockFeature "basic" "basic" optional basic
newfColour      = MockFeature "colour" "Colour" optional basic
newfHighRes     = MockFeature "high-res" "High Resolution" optional basic
newfMedia       = MockFeature "media" "Media" optional (or 2)
newfCamera      = MockFeature "cam" "Camera" optional basic
newfMP3         = MockFeature "mp3" "MP3" optional basic

-- Feature tree
newtreeMobilePhone =
    node newfMobilePhone
    [
        node newfCalls  [],
        node newfGPS    [],
        node newfScreen
        [
           node newfbasic   [],
           node newfColour  [],
           node newfHighRes []
        ],
        node newfMedia
        [
           node newfCamera [],
           node newfMP3    []
        ]
    ]

-----------------------------------------------
-- SETUP
-----------------------------------------------
--
-- Definition of a "Mock" Feature.
data MockFeature = MockFeature {
        myFId          :: FeatureId,
        myFName        :: FeatureName,
        myFCardinality :: FeatureCardinality,
        myGCardinality :: GroupCardinality
    } deriving (Eq)

instance Show MockFeature where
    show f = myFId f

instance SimplifiedFeature MockFeature where
    fId = myFId
    fName = myFName
    fCardinality = myFCardinality
    gCardinality = myGCardinality

-- Function to "override" the default implementation of Tree.show
-- so that the output looks like the old implementation.
show' :: Show a => Tree a -> String
show' t =
    let
        c  = rootLabel t
        cs = subForest t
    in
        case cs of
            [] -> "Leaf " ++ show c
            x  -> "Root " ++ show c ++ " [" ++ intercalate ", " [show' x | x <- cs] ++ "]"
