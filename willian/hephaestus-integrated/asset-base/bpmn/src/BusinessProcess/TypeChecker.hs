module BusinessProcess.TypeChecker (typeChecker) where 

import BusinessProcess.Types

import Data.List

-- import BusinessProcessModel

-- Checks wether a BusinessProcess is consistent, 
-- according to the following rules
--
-- a) there must be just one trasition starting from the start state (why?)
-- b) there must be one or more transitions going to the end state
-- c) all other transitions, apart from start and end, must have at least one incoming transition.
-- d) all other transitions, apart from start and end, must have at least one outgoing transition.
-- 

typeChecker :: BusinessProcess -> Bool
typeChecker bp = (startRule bp) && (endRule bp) && (sequencingRule bp)

startRule bp = ((numberOfIncomingTransitions bp Start) == 1) 

endRule bp = ((numberOfOutgoingTransitions bp End) > 0)
   
sequencingRule bp = seqRuleIn && seqRuleOut 
 where 
  seqRuleIn  = and [hasIncomingTransitions  bp fo | fo <- objects bp, fo /= Start, fo /= End] 
  seqRuleOut = and [hasOutcomingTransitions bp fo | fo <- objects bp, fo /= Start, fo /= End] 


data Position = Incoming | Outgoing
  deriving(Eq)


hasIncomingTransitions  bp fo = not (findTransitions bp fo Incoming  == [])
hasOutcomingTransitions bp fo = not (findTransitions bp fo Outgoing  == [])
 
numberOfIncomingTransitions bp fo = length $ findTransitions bp fo Incoming
numberOfOutgoingTransitions bp fo = length $ findTransitions bp fo Outgoing

-- 
-- Retrieves the set of transitions, either starting from or
-- going to a specific flow object. It is an 
-- an auxiliarly funcition, which helps 
-- the type checker definition.
-- 
-- Nevertheless, I think it should be exposed by the 
-- BusinessProcess.Types module.
--

findTransitions :: BusinessProcess -> FlowObject -> Position -> [(FlowObject, FlowObject, Condition)]
findTransitions bp fo pos = 
 case pos of
  Incoming -> [(i,j,k) | (i,j,k) <- ((<*>) bp),  i == fo]
  Outgoing -> [(i,j,k) | (i,j,k) <- ((<*>) bp),  j == fo]
