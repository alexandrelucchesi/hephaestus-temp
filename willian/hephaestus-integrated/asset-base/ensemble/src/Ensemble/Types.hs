{-# OPTIONS -fglasgow-exts #-}
module Ensemble.Types where 

import UseCaseModel.Types
import BusinessProcess.Types
import RequirementModel.Types
import ComponentModel.Types

import Data.Generics

data Assets = Assets {
  req :: RequirementModel,  
  ucm :: UseCaseModel,
  bpm :: BusinessProcessModel
} deriving(Data, Typeable)

data AssetBase = SPLAssetBase { splAssets :: Assets, mappings :: ComponentModel } 
               | InstanceAssetBase { instanceAssets :: Assets,  buildData :: GeneratedBuildData } deriving (Data, Typeable)


ucModel :: AssetBase -> UseCaseModel
ucModel (SPLAssetBase a _) = ucm a
ucModel (InstanceAssetBase a _) = ucm a

bpModel :: AssetBase -> BusinessProcessModel
bpModel (SPLAssetBase a _) = bpm a
bpModel (InstanceAssetBase a _) = bpm a

reqModel :: AssetBase -> RequirementModel
reqModel (SPLAssetBase a _) = req a
reqModel (InstanceAssetBase a _) = req a

componentModel :: AssetBase -> [ComponentMapping]
componentModel (InstanceAssetBase _ buildData) = components buildData
componentModel (InstanceAssetBase _ buildData) = components buildData
