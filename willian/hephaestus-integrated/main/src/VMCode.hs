-- 
-- This is a command line module 
-- of Hephaestus, target to manage 
-- variability at the source code level.
-- (rbonifacio)
-- 
module Main where 

import System.IO
import System.Environment -- useful for getting program args
import System.Directory
import System.FilePath


import Data.Maybe


import qualified BasicTypes as Core

import ComponentModel.Types
import ComponentModel.Parsers.ParserComponentModel

import Ensemble.Types
import SPL.Types
import SPL.Interpreter

import FeatureModel.Types 
import FeatureModel.Parsers.GenericParser 

import SPL.Transformations.Parsers.XML.XmlConfigurationParser

import ExportProduct (exportSourceCode)

type PropertyValue = (String, String)

-- some references to the rng schemas

fmSchema :: String 
fmSchema = "schema_feature-model.rng"

fcSchema :: String
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String 
ckSchema = "schema-configuration-knowledge.rng"

normalizedSchema cDir sch = cDir </> sch 

main = do 
 cDir <- getCurrentDirectory
 let ns = normalizedSchema cDir
 args <- getArgs 
 
 putStrLn "============================================================================ "
 putStrLn "    )                                                                       "
 putStrLn " ( /(                 )                 )              (          (          "  
 putStrLn " )\\())   (         ( /(     )    (   ( /(   (          )\\         )\\ )   (   " 
 putStrLn "((_)\\   ))\\ `  )   )\\()) ( /(   ))\\  )\\()) ))\\  (    (((_)   (   (()/(  ))\\  " 
 putStrLn " _((_) /((_)/(/(  ((_)\\  )(_)) /((_)(_))/ /((_) )\\   )\\___   )\\   ((_))/((_) "
 putStrLn "| || |(_)) ((_)_\\ | |(_)((_)_ (_))  | |_ (_))( ((_) ((/ __| ((_)  _| |(_))   " 
 putStrLn "| __ |/ -_)| '_ \\)| ' \\ / _` |/ -_) |  _|| || |(_-<  | (__ / _ \\/ _` |/ -_)  " 
 putStrLn "|_||_|\\___|| .__/ |_||_|\\__,_|\\___|  \\__| \\_,_|/__/   \\___|\\___/\\__,_|\\___|  " 
 putStrLn "           |_|                                                               "
 putStrLn "============================================================================="
 putStrLn "                                                         " 
 putStrLn " Please, inform the name of the project file.            "
 putStrLn " It must specify, as pairs key=value, the path to the    "
 putStrLn " input files, such as:                                   "
 putStrLn "  name=projectName                                       "
 putStrLn "  feature-model={abs-path}/featureModel.xml              " 
 putStrLn "  configuration-model={abs-path}/configurationModel.xml  "
 putStrLn "  instance-model={abs-path}/instanceModel.xml            "
 putStrLn "  component-model={abs-path}/componentModel.txt          "
 putStrLn "  source-dir={abs-path}/                                 "
 putStrLn "  target-dir={abs-path}/                                 " 
 putStrLn "============================================================================="
 f <- (if ((length args) == 0)
        then do 
                putStrLn " ProjectFile: " 
                getLine	                 -- read the name of the project file 
        else return $ head args)
 
 s <- readFile f             -- read the file contents
 let l = lines s             -- split the content in several lines

 -- read all properties 
 let ps  = map fromJust (filter (isJust) (map readPropertyValue l))
 
 -- retrieve the specific property values we are interested in
 let name = fromJust (findPropertyValue "name" ps)
 let fmodel = fromJust (findPropertyValue "feature-model" ps)
 let imodel = fromJust (findPropertyValue "instance-model" ps) 
 let ckmodel = fromJust (findPropertyValue "configuration-model" ps)
 let cmodel = fromJust (findPropertyValue "component-model" ps)
 let sourceDir = fromJust (findPropertyValue "source-dir" ps)
 let targetDir = fromJust (findPropertyValue "target-dir" ps)
          
 fmp <- parseFeatureModel  ((ns fmSchema), snd fmodel) (fmFormat $ snd fmodel)
 imp <- parseInstanceModel (ns fcSchema) (snd imodel)  
 ckp <- parseConfigurationKnowledge (ns ckSchema) (snd ckmodel)
 cmp <- parseComponentModel (snd cmodel)   
	  
 let pResults = (fmp, imp, ckp, cmp)
 case pResults of 
    ((Core.Success fm), (Core.Success im), (Core.Success ck), (Core.Success cm)) -> 
        do 
	 putStrLn " Great! We could say that you have done a good job. "
	 putStrLn " All files are syntactically correct.               "
	 putStrLn " Should we build your product [y/n]?                " 
	 
	 proceed <- getChar 
	 
         if proceed == 'y' 
          then do
	   let fc = createFC im
 	   let spl = createSPL fm ck cm
           let product = build fc spl
           let src = snd sourceDir
           let out = snd targetDir
	   exportSourceCode src out product
	   putStrLn $ ("Ok, the output file was genarated at: " ++ out) 
          else putStrLn "Ok, closing your session. To start again, call the main function."

    ((Core.Fail e), _, _, _) -> putStrLn $ "Error parsing the feature model " ++ e 
    (_, (Core.Fail e), _, _) -> putStrLn $ "Error parsing the instance model " ++ e 
    (_, _, (Core.Fail e), _) -> putStrLn $ "Error parsing the configuration model " ++ e
    (_, _, _, (Core.Fail e)) -> putStrLn $ "Error parsing the use case model " ++ e 

 putStrLn "Done!" 
 
  

-- createSPL :: FeatureModel -> ComponentModel -> SPLModel
createSPL fm  ck cm = 
  SPLModel { 
    splFM  = fm, 
    splConfigurationKnowledge = ck,
    splAssetBase = SPLAssetBase { mappings = cm } 
  }

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

fmFormat :: String -> FmFormat
fmFormat fileName 
 | Core.endsWith "sxfm" fileName = SXFM
 | otherwise = FMPlugin