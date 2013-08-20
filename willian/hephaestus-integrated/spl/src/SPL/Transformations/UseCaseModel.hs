-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.UseCaseModel
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Several transformations that instantiate a product's use case model 
-- from a SPL use case model 
--
--
-----------------------------------------------------------------------------
module SPL.Transformations.UseCaseModel 
where

import BasicTypes
import UseCaseModel.Types
import FeatureModel.Types
import SPL.Types
import Ensemble.Types 

import Data.Generics
import Data.Maybe
import Data.List

-- | Transformation that selects SPL use cases
--   from a list of IDs. This transformation deals 
--   with the kind of transformation named 'variability 
--   in function'.
data SelectUseCases = SelectUseCases {
 ucIds :: [Id]
}

instance Transformation SelectUseCases where 
 (<+>) (SelectUseCases ids) spl product = 
        addScenariosToInstance (scs, spl, product) 
        where scs = concat $ map ucScenarios [uc | uc <- useCases (ucModel $ splAssetBase spl), ucId uc `elem` ids]

-- | Transformation that selects SPL scenarios 
--   from a list of IDs. This transformation deals 
--   with the kind of transformation named 'variability 
--   in function'.

data SelectScenarios = SelectScenarios  { 
 scIds :: [Id]  
}

instance Transformation SelectScenarios where 
 (<+>) (SelectScenarios ids) spl product =
        addScenariosToInstance (scs, spl, product)
        where scs = [s | s <- splScenarios spl, scId s `elem` ids]

-- | Transformation that binds scenario parameters to a selection of 
--   features. It deals with the kind of transformation named 
--   'variability in data'. 

data BindParameter = BindParameter { 
 pmtId :: Id,      -- ^ formal parameter of scenarios
 featureId :: Id   -- ^ feature identifier
}

instance Transformation BindParameter where 
 (<+>) (BindParameter pid fid) spl product = bindParameter steps (parenthesize options) pid product
  where 
   steps = [s | s <- ucmSteps (ucModel $ instanceAssetBase product), s `refers` pid]
   options = concat (map featureOptionsValues [f | f <- flatten (fcTree (fc product)), fId (fnode f) == fid]) 
   bindParameter [] o pid p = p
   bindParameter (s:ss) o pid p = bindParameter ss o pid (gReplaceParameterInScenario (sId s) pid o p) 

-- | Transformation that evaluates a list of aspects. 
--   This transformation deals with the kind of variability 
--   named 'variability in control flow'.

data EvaluateAspects = EvaluateAspects { 
 aspectIds :: [Id]  -- ^ Ids of the aspects that will be evaluated  
}

instance Transformation EvaluateAspects where 
 (<+>) (EvaluateAspects ids) spl product = evaluateListOfAdvice as product
  where 
   as = concat [advices a | a <- aspects (ucModel $ splAssetBase spl), (aspectId a) `elem` ids]

-- evaluate a list of advices
evaluateListOfAdvice :: [Advice] -> InstanceModel -> InstanceModel
evaluateListOfAdvice [] p = p
evaluateListOfAdvice (x:xs) p = evaluateListOfAdvice xs (genEvaluateAdvice x p)


evaluateAdvice :: Advice -> Scenario -> Scenario
evaluateAdvice a s = foldl (compose a) s pcs 
 where pcs = pointCut a

compose :: Advice -> Scenario  -> StepRef -> Scenario
compose adv sc pc = 
 if (matches pc sc) 
  then sc { steps = (compose' (advType adv)) aFlow sFlow }
  else sc
 where 
  compose' (Before) = concatBefore (match pc)
  compose' (After)  = concatAfter  (match pc)
  compose' (Around) = concatAround (match pc) proceed
  aFlow = aspectualFlow adv
  sFlow = steps sc

-- if composing a before advice.
-- compose (BeforeAdvice _ flow) sc pc = 
--  if (matches pc sc)  
--   then sc { steps = concatBefore (match pc) flow (steps sc) }
--   else sc 

-- -- if composing an after advice
-- compose (AfterAdvice _ flow) sc pc = 
--  if (matches pc sc)
--   then sc { steps = concatAfter (match pc) flow (steps sc) }
--   else sc

-- evaluate a single advice. This function 
-- implements the weaving process at the lower 
-- granularity level: a flow. 
-- evaluateAdvice :: Advice -> Scenario -> Scenario
-- evaluateAdvice a s = s { steps = (evaluateAdvice' p af sf) }
--  where 
--   p = pointCut a
--   af = aspectualFlow a
--   sf = steps s
--   fn = case a of 
--         BeforeAdvice _ _-> concatBefore
--         AfterAdvice _ _-> concatAfter
--   evaluateAdvice' [] af sf = sf
--   evaluateAdvice' (x:xs) af sf = evaluateAdvice' xs af (fn (match x) af sf) 

-- this is the generic function for evaluating 
-- an advice. It follows the Scrap Your Boilerplate (SYB)
-- pattern. 
genEvaluateAdvice :: Advice -> InstanceModel -> InstanceModel  
genEvaluateAdvice a = everywhere (mkT (evaluateAdvice a))-- everywhere (mkT (evaluateAdvice a))

-- just an auxiliarly function for checking if a 
-- step refers to a parameter. this function might 
-- be parameterized with different delimiters.   
refers :: Step -> Id -> Bool
refers s pid = 
 let 
  w = "{" ++ pid ++ "}" 
  wExists = existsWord w
 in (wExists (action s)) || (wExists (state s)) || (wExists (response s))
    

replaceParameterInScenario :: Id -> String -> String -> Scenario -> Scenario
replaceParameterInScenario sid fp ap scn = 
 let sts = steps scn 
 in scn { steps = map (replaceStringInStep sid fp ap) sts }

-- just an auxiliarly function for replacin a string in a step. 
-- actually, it replaces a string at any place of a step: action, 
-- condition, or response. 
replaceStringInStep :: Id -> String -> String -> Step -> Step
replaceStringInStep sid old new step = 
 if (sId step /= sid) 
  then step
  else step { action = rfn a, state = rfn s, response = rfn r }  
   where 
    a = action step
    s = state step
    r = response step  
    rfn = replaceString ("{"++old++"}") new
 
-- this is the generic function for replacing a string into a step (identified 
-- by 'sid'). It follows the SYB pattern. 
gReplaceParameterInScenario :: Id -> String -> String -> InstanceModel -> InstanceModel
gReplaceParameterInScenario sid fp ap = everywhere (mkT (replaceParameterInScenario sid fp ap)) 


-- this is a map function that adds a list of scenarios 
-- to a use case model.
addScenariosToInstance :: ([Scenario], SPLModel, InstanceModel) -> InstanceModel
addScenariosToInstance ([], spl, product) = product
addScenariosToInstance ((s:ss), spl, product) = addScenariosToInstance (ss, spl, product') 
 where 
  product' = addScenarioToInstance (s, sUseCase, product) 
  sUseCase = findUseCaseFromScenario (useCases (ucModel $ splAssetBase spl)) s

-- add a single scenario to a use case model.
addScenarioToInstance :: (Scenario, Maybe UseCase, InstanceModel) -> InstanceModel
addScenarioToInstance (s, Nothing, product) =  error "Scenario not declared within a use case"
addScenarioToInstance (s, (Just sUseCase), product)  = 
 let
  pUseCase = [u | u <- useCases (ucModel $ instanceAssetBase product), (ucId sUseCase) == (ucId u)]
  eUseCase = (emptyUseCase sUseCase) { ucScenarios = [s] } 
 in case pUseCase of
     []  -> gAddUseCase eUseCase product
     [u] -> gAddScenario (ucId u) s product

-- add or replace a scenarion to a use case. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceScenario :: Id -> Scenario -> UseCase -> UseCase
addOrReplaceScenario i sc uc =
 if (ucId uc == i) 
  then 
   let scs = ucScenarios uc 
   in uc { ucScenarios = [s | s <- scs, s /= sc] ++ [sc]}
  else uc 

-- add or replace a use case to a use case model. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceUseCase :: UseCase -> UseCaseModel -> UseCaseModel
addOrReplaceUseCase uc ucModel = 
 let ucs = useCases ucModel 
 in ucModel { useCases = [u | u <- ucs, u /= uc ] ++ [uc]}

-- this is the generic function for adding a scenario.
-- it follows the SYB pattern. 
gAddScenario :: Id -> Scenario -> InstanceModel -> InstanceModel
gAddScenario i s = everywhere (mkT (addOrReplaceScenario i s))

-- this is the generic function for adding a use case. 
-- it follows the SYB pattern. 
gAddUseCase :: UseCase -> InstanceModel -> InstanceModel 
gAddUseCase u = everywhere (mkT (addOrReplaceUseCase u))

emptyUseCase :: UseCase -> UseCase
emptyUseCase uc = uc { ucScenarios = [] }

splScenarios spl = ucmScenarios (ucModel $ splAssetBase spl)
   
instance Show SelectScenarios where 
 show (SelectScenarios ids) = "selectScenarios " ++ (show ids)

instance Show EvaluateAspects where 
 show (EvaluateAspects ids) = "evaluateAspects " ++ (show ids) 

instance Show SelectUseCases where 
 show (SelectUseCases ids) = "selectUseCases " ++ (show ids)
 
instance Show BindParameter where 
 show (BindParameter p f) = "bindParameter " ++ p ++ " to " ++ f 
   
   
   