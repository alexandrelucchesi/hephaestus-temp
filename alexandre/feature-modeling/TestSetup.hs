module TestSetup where

import FeatureModel.Types as Old hiding (eval)
import FeatureModel.NewTypes.Types as New hiding (Feature)
import FeatureModel.Logic as New hiding (eval)

import Prelude hiding (or)

import Data.List hiding (or)
import Data.Tree

-----------------------------------------------
-- OLD
-----------------------------------------------

-- SAMPLE 01 ----------------------------------
ftree1_old =
    Root (Feature "F1" "F1" Mandatory "F1" AlternativeFeature [])
    [
        Root (Feature "F2" "F2" Mandatory "F2" AlternativeFeature [])
        [
            Leaf (Feature "F4" "F4" Mandatory "F4" AlternativeFeature [])
        ],
        Root (Feature "F3" "F3" Mandatory "F3" AlternativeFeature [])
        [
            Leaf (Feature "F5" "F5" Mandatory "F5" AlternativeFeature [])
        ]
    ]

fmTree1_old = Old.FeatureModel ftree1_old []

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
    node (MockFeature "F1" "F1" mandatory alternative)
    [
        node (MockFeature "F2" "F2" mandatory alternative)
        [
            node (MockFeature "F4" "F4" mandatory alternative) []
        ],
        node (MockFeature "F3" "F3" mandatory alternative)
        [
            node (MockFeature "F5" "F5" mandatory alternative) []
        ]
    ]

fmTree1_new = New.FeatureModel ftree1_new []

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

equals :: SimplifiedFeature a => a -> Old.Feature -> Bool
equals new old =
    let newId   = New.fId new
        newName = New.fName new
        newCard = New.fCardinality new
        newGrp  = New.gCardinality new
        oldId   = Old.fId old
        oldName = Old.fName old
        oldCard = Old.fType old
        oldGrp  = Old.groupType old
        in newId == oldId
           && newName == oldName
           && equalsCard newCard oldCard
           && equalsGrp newGrp oldGrp
    where
    equalsCard (Cardinality 0 1) Optional  = True
    equalsCard (Cardinality 1 1) Mandatory = True
    equalsCard _ _ = False
    equalsGrp (Cardinality 0 0) BasicFeature = True
    equalsGrp (Cardinality 1 1) AlternativeFeature = True
    equalsGrp (Cardinality 1 _) OrFeature = True
    equalsGrp _ _ = False

