-- @author Alexandre Lucchesi
--
-- This module shows the creation of a sample Feature Model.

module FMMobile where

import BasicFeature
import FeatureModel.NewTypes.Types

-- Features
fMobilePhone = BasicFeature "mobile-phone" "Mobile Phone" Mandatory Basic Green
fCalls       = BasicFeature "calls" "Calls" Mandatory Basic Yellow
fGPS         = BasicFeature "gps" "GPS" Optional Basic Green
fScreen      = BasicFeature "screen" "Screen" Mandatory Alternative Red 
fBasic       = BasicFeature "basic" "Basic" Optional Basic Red
fColour      = BasicFeature "colour" "Colour" Optional Basic Yellow
fHighRes     = BasicFeature "high-res" "High Resolution" Optional Basic Red
fMedia       = BasicFeature "media" "Media" Optional (Or 2) Green
fCamera      = BasicFeature "cam" "Camera" Optional Basic Yellow
fMP3         = BasicFeature "mp3" "MP3" Optional Basic Green

-- Feature tree
treeMobilePhone =
    node fMobilePhone [
                        node fCalls  [],
                        node fGPS    [],
                        node fScreen [
                                       node fBasic   [],
                                       node fColour  [],
                                       node fHighRes []
                                     ],
                        node fMedia  [
                                       node fCamera [],
                                       node fMP3    []
                                     ]
                      ]

-- Feature model
fmMobilePhone = FeatureModel treeMobilePhone []

