-- @author Alexandre Lucchesi
-- 
-- This module illustrates the creation of a Custom feature.
--
-- Basically, one must import the module 'Types' and make the custom 
-- feature an instance of SimplifiedFeature typeclass.

module BasicFeature where

import Core.Types

-- Custom feature definition.
-- The developer is free to define its own attributes and business logic.
data FeatureType = Optional | Mandatory
                 deriving (Eq, Show)

data GroupType = Basic
               | Alternative
               | Or Int -- max cardinality
               deriving (Eq, Show)

-- Sample dummy attribute.
data Color = Red | Green | Yellow
             deriving (Eq, Show)

data BasicFeature = BasicFeature {
                        fId'   :: FeatureId,
                        fName' :: FeatureName,
                        fType  :: FeatureType,
                        fGroup :: GroupType,
                        myAttr :: Color
                    } deriving (Eq, Show)

-- Then, it just have to make its new custom feature an instance of 
-- 'SimplifiedFeature', which is what Hephaestus needs to work.
instance SimplifiedFeature BasicFeature where
    fId   = fId'
    fName = fName'
    fCardinality f =
        case fType f of
            Optional  -> Cardinality 0 1
            Mandatory -> Cardinality 1 1
    gCardinality f =
        case fGroup f of
            Basic       -> Cardinality 0 0
            Alternative -> Cardinality 1 1
            (Or max)    -> Cardinality 1 max

