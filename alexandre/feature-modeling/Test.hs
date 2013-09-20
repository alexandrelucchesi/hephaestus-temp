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
assertTrue_treeMobilePhone = Old.featureToPropositionalLogic treeMobilePhone_old == New.featureToPropositionalLogic treeMobilePhone_new
assertTrue_fmMobilePhone = Old.fmToPropositionalLogic fmMobilePhone_old == New.fmToPropositionalLogic fmMobilePhone_new

-----------------------------------------------
-- OLD
-----------------------------------------------

-- SAMPLE 01 ----------------------------------
ftree1_old =
    Root (Feature "F1" "F1" Mandatory "F1" BasicFeature [])
    [
        Root (Feature "F2" "F2" Mandatory "F2" BasicFeature [])
        [
            Leaf (Feature "F4" "F4" Mandatory "F4" BasicFeature [])
        ],
        Root (Feature "F3" "F3" Mandatory "F3" BasicFeature [])
        [
            Leaf (Feature "F5" "F5" Mandatory "F5" BasicFeature [])
        ]
    ]

-- SAMPLE 02 ----------------------------------
-- Features
oldfMobilePhone = Feature "mobile-phone" "Mobile Phone" Mandatory "Mobile Phone" BasicFeature []
oldfCalls       = Feature "calls" "Calls" Mandatory "Calls" BasicFeature []
oldfGPS         = Feature "gps" "GPS" Optional "GPS" BasicFeature []
oldfScreen      = Feature "screen" "Screen" Mandatory "Screen" AlternativeFeature []
oldfBasic       = Feature "basic" "Basic" Optional "Basic" BasicFeature []
oldfColour      = Feature "colour" "Colour" Optional "Colour" BasicFeature []
oldfHighRes     = Feature "high-res" "High Resolution" Optional "High Resolution" BasicFeature []
oldfMedia       = Feature "media" "Media" Optional "Media" OrFeature []
oldfCamera      = Feature "cam" "Camera" Optional "Camera" BasicFeature []
oldfMP3         = Feature "mp3" "MP3" Optional "MP3" BasicFeature []

-- Feature tree
treeMobilePhone_old =
    Root oldfMobilePhone
    [
        Leaf oldfCalls,
        Leaf oldfGPS,
        Root oldfScreen
        [
           Leaf oldfBasic,
           Leaf oldfColour,
           Leaf oldfHighRes
        ],
        Root oldfMedia
        [
           Leaf oldfCamera,
           Leaf oldfMP3
        ]
    ]

fmMobilePhone_old = Old.FeatureModel treeMobilePhone_old []

-----------------------------------------------
-- NEW
-----------------------------------------------

-- SAMPLE 01 ----------------------------------
ftree1_new =
    node (MockFeature "F1" "F1" mandatory basic)
    [
        node (MockFeature "F2" "F2" mandatory basic)
        [
            node (MockFeature "F4" "F4" mandatory basic) []
        ],
        node (MockFeature "F3" "F3" mandatory basic)
        [
            node (MockFeature "F5" "F5" mandatory basic) []
        ]
    ]

-- SAMPLE 02 ----------------------------------
-- Features
newfMobilePhone = MockFeature "mobile-phone" "Mobile Phone" mandatory basic
newfCalls       = MockFeature "calls" "Calls" mandatory basic
newfGPS         = MockFeature "gps" "GPS" optional basic
newfScreen      = MockFeature "screen" "Screen" mandatory alternative
newfBasic       = MockFeature "basic" "Basic" optional basic
newfColour      = MockFeature "colour" "Colour" optional basic
newfHighRes     = MockFeature "high-res" "High Resolution" optional basic
newfMedia       = MockFeature "media" "Media" optional (or 2)
newfCamera      = MockFeature "cam" "Camera" optional basic
newfMP3         = MockFeature "mp3" "MP3" optional basic

-- Feature tree
treeMobilePhone_new =
    node newfMobilePhone
    [
        node newfCalls  [],
        node newfGPS    [],
        node newfScreen
        [
           node newfBasic   [],
           node newfColour  [],
           node newfHighRes []
        ],
        node newfMedia
        [
           node newfCamera [],
           node newfMP3    []
        ]
    ]

fmMobilePhone_new = New.FeatureModel treeMobilePhone_new []

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
