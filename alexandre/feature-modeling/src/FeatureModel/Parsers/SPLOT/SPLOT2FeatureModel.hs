module FeatureModel.Parsers.SPLOT.SPLOT2FeatureModel where 

import qualified FeatureModel.Types as FM
import FeatureModel.Parsers.SPLOT.AbsSPLOT


modeloSPLOT = SPLOT (FeatureModel (Feature (FName2 (Ident "r")) [SetRelation (FName2 (Ident "r_6")) (Cardinality 2 1) [GroupedFeature (FName2 (Ident "r_6_7")) [BinRelation (Cardinality 1 1) (SolitaryFeature (FName2 (Ident "r_6_7_9")) [SetRelation (FName2 (Ident "r_6_7_9_10")) (Cardinality 3 1) [LGroupedFeature (FName2 (Ident "r_6_7_9_10_11")),LGroupedFeature (FName2 (Ident "r_6_7_9_10_15")),LGroupedFeature (FName2 (Ident "r_6_7_9_10_16"))]])],GroupedFeature (FName2 (Ident "r_6_8")) [BinRelation (Cardinality 1 1) (SolitaryFeature (FName2 (Ident "r_6_8_19")) [SetRelation (FName2 (Ident "r_6_8_19_20")) (Cardinality 2 1) [LGroupedFeature (FName2 (Ident "r_6_8_19_20_21")),LGroupedFeature (FName2 (Ident "r_6_8_19_20_23"))]])]]]) [Require (FName2 (Ident "r_6_7_9_10_16")) (FName1 (Ident "constraint_1")) (FName2 (Ident "r_6_8_19_21"))])


modelo1=SPLOT(FeatureModel (Feature (FName2 (Ident "r"))[]) [])
modelo2=SPLOT(FeatureModel (Feature (FName2 (Ident "r"))[BinRelation (Cardinality 1 1) (LSolitaryFeature (FName2 (Ident "r_6_7")))]) [])


splotToFeatureModel :: SPLOTModel -> FM.FeatureModel
splotToFeatureModel (SPLOT (FeatureModel r cs)) = 
  let
   fmTree = featureTreeSPLOT2FeatureTree r-- foi decompor a árvore de features
   constraints = map constraintToExpression cs -- para iniciar traduzir primeiro fmTreemap constraintToFeatureModel cs
  in FM.FeatureModel fmTree constraints

featureTreeSPLOT2FeatureTree :: Feature -> FM.FeatureTree  
featureTreeSPLOT2FeatureTree (Feature n cs) =  -- n = FName cs =[Child]
 FM.Root r cs'
 where 
  r = createFeature n FM.Mandatory (group cs) -- group = groupType
  cs' = case group cs of 
         FM.BasicFeature -> (map child2Feature cs)
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
  
child2Feature :: Child -> FM.FeatureTree
child2Feature (SetRelation _ _ gfs) = error "could not call Set Relation at others..." 

child2Feature (BinRelation c (LSolitaryFeature n)) = FM.Leaf r
 where 
  r = createFeature n (ftype c) FM.BasicFeature

child2Feature (BinRelation c (SolitaryFeature n cs)) = FM.Root r cs'
 where 
  r = createFeature n (ftype c) (group cs) 
  cs' = case group cs of 
         FM.BasicFeature -> (map child2Feature cs)
         otherwise -> let 
                       [(SetRelation _ _ gs)] = cs 
                      in map groupedFeature2Feature gs  
 
groupedFeature2Feature (GroupedFeature n cs) = FM.Root r (map child2Feature cs)
 where 
  r = createFeature n FM.Optional (group cs) 
  
groupedFeature2Feature (LGroupedFeature n) = FM.Leaf r 
 where 
  r = createFeature n FM.Optional FM.BasicFeature
  

createFeature :: FName -> FM.FeatureType -> FM.GroupType -> FM.Feature
createFeature (FName2 (Ident n)) t g = FM.Feature n n t "" g [] 
 
ftype :: Cardinality -> FM.FeatureType
ftype (Cardinality 1 1) = FM.Mandatory
ftype (_) = FM.Optional 

group :: [Child] -> FM.GroupType
group [(SetRelation _ c _)] = 
 case c of 
  (Cardinality 1 1) -> FM.AlternativeFeature
  (Cardinality _ 1) -> FM.OrFeature
group _ = FM.BasicFeature  


