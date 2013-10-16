module FeatureModel.Parsers.SPLOT.AbsSPLOT where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data SPLOTModel =
   SPLOT FeatureModel
  deriving (Eq,Ord,Show)

data FeatureModel =
   FeatureModel Feature [Constraint]
  deriving (Eq,Ord,Show)

data Feature =
   Feature FName [Child]
  deriving (Eq,Ord,Show)

data Child =
   SetRelation FName Cardinality [GroupedFeature]
 | BinRelation Cardinality SolitaryFeature
  deriving (Eq,Ord,Show)

data GroupedFeature =
   GroupedFeature FName [Child]
 | LGroupedFeature FName
  deriving (Eq,Ord,Show)

data SolitaryFeature =
   SolitaryFeature FName [Child]
 | LSolitaryFeature FName
  deriving (Eq,Ord,Show)

data Cardinality =
   Cardinality Integer Integer
  deriving (Eq,Ord,Show)

data Constraint =
   Require FName FName FName
 | Exclude FName FName FName
  deriving (Eq,Ord,Show)

data FName =
   FName1 Ident
 | FName2 Ident
  deriving (Eq,Ord,Show)

