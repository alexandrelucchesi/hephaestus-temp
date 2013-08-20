-----------------------------------------------------------------------------
-- |
-- Module      :  ConfigurationKnowledge.StaticChecker
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009, 2010
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Static checker for the configuration knowledge in Haskell.
--
-----------------------------------------------------------------------------
module SPL.StaticChecker (ckStaticChecker)

where 

import qualified BasicTypes as Core

import SPL.Types

import FeatureModel.Types
import FeatureModel.FMTypeChecker

import Funsat.Types
import Funsat.Solver
import Funsat.Resolution


ckStaticChecker :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> [ErrorMessage]
ckStaticChecker fm fc  ck = 
 case fmTypeChecker fm of 
   Success -> (checkInvalidFeatureReferences fm ck) ++ (checkUnSATExpressions fm ck) ++ (checkRequiredAndProvided fc ck)
   Fail err -> err 

-- This function checks which expressions
-- declared in the configuration knowledge 
-- refer to features not defined in the 
-- feature model.

checkInvalidFeatureReferences :: FeatureModel -> ConfigurationKnowledge -> [ErrorMessage]
checkInvalidFeatureReferences fm [] = []
checkInvalidFeatureReferences fm (x:xs) = (checkConstraints (addExp fm x)) ++ (checkInvalidFeatureReferences fm xs) 


-- This function checks which expressions
-- declared in the configuration knowledge
-- could NOT be satisfied by the feature model. 

checkUnSATExpressions :: FeatureModel -> ConfigurationKnowledge -> [ErrorMessage]                

checkUnSATExpressions fm ck = ["Expression " ++ (show $ expression c) ++ " is UNSAT regarding the feature model." 
                              | c <- ck 
                              , (isSatisfiable (addExp fm c)) == False
                              ]

-- just one auxilarly function for introducing 
-- an expression of a configuration item  into 
-- the feature model. 
addExp fm c = addConstraint fm (expression c)

checkRequiredAndProvided :: FeatureConfiguration -> ConfigurationKnowledge -> [ErrorMessage] 

checkRequiredAndProvided fc ck = 
 let 
  cis = [c | c <-ck , constrained c, eval fc (expression c)] -- valid configurations with embedded constraints
  p = foldAnd [provided c | c <- cis]                        -- conjunction of the provided clauses in the entire build
  allConstraints = [(c, (required c) |=> p) | c <- cis]      -- requied implies provided of the entire build
 in  
  ["Configuration requiring " ++ (show $ required (fst c)) ++ " is UNSAT regarding the provided clauses of the product."
  | c <- allConstraints
  , (checkSAT (snd c)) == False
  ]


checkSAT e = isSAT $ solve1 (dimacsFormat (toCNFExpression e))  