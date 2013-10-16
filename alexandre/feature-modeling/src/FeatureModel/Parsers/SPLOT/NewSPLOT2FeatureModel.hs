module FeatureModel.Parsers.SPLOT.NewSPLOT2FeatureModel where 

import qualified FeatureModel.Types as FM (FeatureExpression(..))
import qualified FeatureModel.NewTypes.Types as FM
import FeatureModel.Parsers.SPLOT.AbsSPLOT
import TestSetup (MockFeature(..))

modeloSPLOT = SPLOT (FeatureModel (Feature (FName2 (Ident "r")) [SetRelation (FName2 (Ident "r_6")) (Cardinality 2 1) [GroupedFeature (FName2 (Ident "r_6_7")) [BinRelation (Cardinality 1 1) (SolitaryFeature (FName2 (Ident "r_6_7_9")) [SetRelation (FName2 (Ident "r_6_7_9_10")) (Cardinality 3 1) [LGroupedFeature (FName2 (Ident "r_6_7_9_10_11")),LGroupedFeature (FName2 (Ident "r_6_7_9_10_15")),LGroupedFeature (FName2 (Ident "r_6_7_9_10_16"))]])],GroupedFeature (FName2 (Ident "r_6_8")) [BinRelation (Cardinality 1 1) (SolitaryFeature (FName2 (Ident "r_6_8_19")) [SetRelation (FName2 (Ident "r_6_8_19_20")) (Cardinality 2 1) [LGroupedFeature (FName2 (Ident "r_6_8_19_20_21")),LGroupedFeature (FName2 (Ident "r_6_8_19_20_23"))]])]]]) [Require (FName2 (Ident "r_6_7_9_10_16")) (FName1 (Ident "constraint_1")) (FName2 (Ident "r_6_8_19_21"))])

modelo1 = SPLOT(FeatureModel (Feature (FName2 (Ident "r"))[]) [])

modelo2 = SPLOT(FeatureModel (Feature (FName2 (Ident "r"))[BinRelation (Cardinality 1 1) (LSolitaryFeature (FName2 (Ident "r_6_7")))]) [])

--splotToFeatureModel :: SPLOTModel -> FM.FeatureModel a
splotToFeatureModel (SPLOT (FeatureModel r cs)) = 
  let
   fmTree = featureTreeSPLOT2FeatureTree r-- foi decompor a árvore de features
   constraints = map constraintToExpression cs -- para iniciar traduzir primeiro fmTreemap constraintToFeatureModel cs
  in FM.FeatureModel fmTree constraints

----featureTreeSPLOT2FeatureTree :: Feature -> FM.FeatureTree a
featureTreeSPLOT2FeatureTree (Feature n cs) =  -- n = FName cs =[Child]
-- FM.Root r cs'
 FM.node r cs'
 where 
  r = createFeature n FM.mandatory (group cs) -- group = groupType
  cs' = case group cs of 
         (FM.Cardinality 0 0) -> map child2Feature cs
         otherwise -> let 
                       [(SetRelation _ _ gs)] = cs 
                      in map groupedFeature2Feature gs  


constraintToExpression :: Constraint -> FM.FeatureExpression
constraintToExpression (Require f1 _ f2) = FM.Or (FM.Not(FM.FeatureRef (fname f1))) (FM.FeatureRef (fname f2))
constraintToExpression (Exclude f1 f2 _) = FM.Not (FM.And (FM.FeatureRef (fname f1)) (FM.FeatureRef (fname f2)))
{-
  (Exclude f1 f2 _) -> FM.Not (FM.And ((FM.FeatureRef s1), (FM.FeatureRef s2)))
 where
   s1= fname f1
   s2= fname f2-}

fname ::FName -> String
fname (FName1 (Ident s)) = s
fname (FName2 (Ident s)) = s
  
--child2Feature :: Child -> FM.FeatureTree a
child2Feature (SetRelation _ _ gfs) = error "could not call Set Relation at others..." 

child2Feature (BinRelation c (LSolitaryFeature n)) = FM.node r []
 where 
  r = createFeature n (ftype c) FM.basic

child2Feature (BinRelation c (SolitaryFeature n cs)) = FM.node r cs'
 where 
  r = createFeature n (ftype c) (group cs) 
  cs' = case group cs of 
         (FM.Cardinality 0 0) -> map child2Feature cs
         otherwise -> let 
                       [(SetRelation _ _ gs)] = cs 
                      in map groupedFeature2Feature gs  
 
groupedFeature2Feature (GroupedFeature n cs) = FM.node r (map child2Feature cs)
 where 
  r = createFeature n FM.optional (group cs) 
  
groupedFeature2Feature (LGroupedFeature n) = FM.node r [] 
 where 
  r = createFeature n FM.optional FM.basic

--createFeature :: FName -> FM.FeatureCardinality -> FM.GroupCardinality -> FM.Feature a
createFeature (FName2 (Ident n)) t g = MockFeature n n t g
 
ftype :: Cardinality -> FM.FeatureCardinality
ftype (Cardinality 1 1) = FM.mandatory
ftype (_) = FM.optional

group :: [Child] -> FM.GroupCardinality
group [(SetRelation _ c _)] = 
 case c of 
  (Cardinality 1 1) -> FM.alternative
  (Cardinality _ 1) -> FM.or 1
group _ = FM.basic  

