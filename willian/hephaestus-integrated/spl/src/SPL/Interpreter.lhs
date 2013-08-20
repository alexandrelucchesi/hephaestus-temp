\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConfigurationKnowledge.Interpreter
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Configuration Knowledge interpreter in Haskell.
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}

module SPL.Interpreter (build)
where

import ComponentModel.Types
import Ensemble.Types
import UseCaseModel.Types
import BusinessProcess.Types
import RequirementModel.Types
import SPL.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, eval)
import FeatureModel.FCTypeChecker (validInstance)

-- | Instantiates a product from the input models. 
--   In more details, it calls each transformation ('tasks', obtained 
--   from the configuration knowledge 'ck') that should be applied to 
--   the given feature configuration ('fc').

build :: FeatureConfiguration         -- ^ selection of features, which characterizes the product
      -> SPLModel                     -- ^ SPL assets
      -> InstanceModel                -- ^ resulting instance of the build process
build fc spl = stepRefinement ts spl emptyInstance
 where 
  ts            = tasks (splConfigurationKnowledge spl) fc
  ucmodel       = ucModel $ splAssetBase spl
  bpmodel       = bpModel $ splAssetBase spl
  emptyUCM      = ucmodel { useCases = [] , aspects = [] }
  emptyBPM      = bpmodel { processes = [] }
  emptyReq      = RM { reqs = [] }
  emptyInstance = InstanceModel fc (InstanceAssetBase (Assets emptyReq emptyUCM emptyBPM) (GeneratedBuildData [] [] []))
 	
tasks :: ConfigurationKnowledge -> FeatureConfiguration -> [GenT]
tasks ck fc = concat [transformations c | c <- ck, eval fc (expression c)]

stepRefinement :: [GenT] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splModel instanceModel = instanceModel
stepRefinement ((GenT t):ts) splModel instanceModel = stepRefinement ts splModel ((<+>) t splModel instanceModel)
 
\end{code}
    

 