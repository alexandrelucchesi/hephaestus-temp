module Main where 

import System.IO
import System.Environment -- useful for getting program args
import System.Directory
import System.FilePath


import Maybe

import SPL.Types
import SPL.Interpreter

import qualified BasicTypes as Core
 
import FeatureModel.Types 
import FeatureModel.Parsers.GenericParser 
import SPL.Transformations.Parsers.XML.XmlConfigurationParser

type PropertyValue = (String, String)

fmSchema :: String 
fmSchema = "schema_feature-model.rng"

fcSchema :: String
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String 
ckSchema = "schema-configuration-knowledge.rng"

normalizedSchema cDir sch = cDir </> sch 

main = do error "the implementation is missing. "
 -- cDir <- getCurrentDirectory
 -- let ns = normalizedSchema cDir
 -- args <- getArgs 
 -- case args of 
 --  [f] -> do
 --          s <- readFile f 
 --      	  let l   = lines s
 --          let ps  = map fromJust (filter (isJust) (map readPropertyValue l))
 --          let fmf  = fromJust (findPropertyValue "feature-model" ps)
 --          let imf  = fromJust (findPropertyValue "instance-model" ps) 
 --          let cmf  = fromJust (findPropertyValue "configuration-model" ps)
 --          let ucmf = fromJust (findPropertyValue "usecase-model" ps)
          
 --          fmp <- {-# SCC "FMparser" #-} parseFeatureModel  ((ns fmSchema), snd fmf) FMPlugin
 --          imp <- {-# SCC "IMparser" #-} parseInstanceModel (ns fcSchema) (snd imf)  
 --          cmp <- {-# SCC "CKpaerser" #-} parseConfigurationKnowledge (ns ckSchema) (snd cmf)  	       	  
	  
 --          let pResults = (fmp, imp, cmp)
 --          case pResults of 
 --           ((Core.Success fm), (Core.Success im), (Core.Success cm)) -> do 
 -- 	   	     	  	   		      	  let fc = createFC im
 --                                                          let ts = {-# SCC "eval" #-} concat [transformations c | c <- cm, eval fc (expression c)]
 --      							  print $ "Done! Number of transformations is " ++ show (length ts)
 --           (_, (Core.Fail e), _) -> error e 
 -- 	   (_, _, (Core.Fail e)) -> error e

 --  otherwise -> error "You should provide the absolute path of a property file"


-- given a String s, it returns just a property, 
-- if s matches "key=value". Otherwise, it returns 
-- Nothing.
readPropertyValue :: String -> Maybe PropertyValue
readPropertyValue s =
 let p = break (== '=') s
 in case p of 
     ([], _) -> Nothing
     (k , v) -> Just (k, tail v)  

findPropertyValue:: String -> [PropertyValue] -> Maybe PropertyValue  
findPropertyValue k [] = Nothing
findPropertyValue k (x:xs) =   
 if (k == fst x) then Just x
 else findPropertyValue k xs 

createFC im = FeatureConfiguration im