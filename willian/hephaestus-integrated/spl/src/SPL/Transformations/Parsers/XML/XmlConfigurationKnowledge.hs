module SPL.Transformations.Parsers.XML.XmlConfigurationKnowledge
where

import BasicTypes

import SPL.Types

import SPL.Transformations.ComponentModel
import SPL.Transformations.UseCaseModel

import UseCaseModel.Types

import FeatureModel.Parsers.Expression
import FeatureModel.Types hiding (Success,Fail)

import Text.XML.HXT.Core

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

import Data.List
         
data XmlConfigurationKnowledge = XmlConfigurationKnowledge {
      xmlConfigurations :: [XmlConfiguration]
} deriving(Show)  
	
data XmlConfiguration = XmlConfiguration {
      xmlExpression:: String,
      xmlTransformations :: [XmlTransformation],
      xmlRequired :: Maybe String, 
      xmlProvided :: Maybe String
} deriving(Show)  	
	
data XmlTransformation = XmlTransformation {
      tName :: String,
      tArgs :: String
} deriving(Show)

xml2ConfigurationKnowledge :: XmlConfigurationKnowledge -> ParserResult ConfigurationKnowledge
xml2ConfigurationKnowledge ck = 
 let 
   cs  = xmlConfigurations ck
   mcs = map xml2Configuration cs
 in 
  if and [isSuccess c | c <- mcs]
   then Success [ci | (Success ci) <- mcs]
   else Fail (unwords [showError e | e <- mcs, isSuccess e == False])

xml2Configuration :: XmlConfiguration -> ParserResult Configuration 
xml2Configuration c =
 let 
  pe = parse parseExpression "" (xmlExpression c)
  ts = map xml2Transformation (xmlTransformations c)
  pr = parseConstraint (xmlRequired c)
  pp = parseConstraint (xmlProvided c)  
  pl = [pe, pr, pp] 
 in 
  if and [isSuccess t | t <-ts] then 
   let pts = [a | (Success a) <- ts]
   in 
    case pl of
      [Right exp, Right req, Right prov] -> Success (ConstrainedConfiguration { expression = exp
     	    					                              , transformations = pts
					        	 		      , required = req 
					        			      , provided = prov })

      [Right exp, _, _] -> Success (Configuration { expression = exp
    	   	      	 	 		  , transformations = pts})     
  
      otherwise -> Fail ("Error parsing configuration item with "    ++ 
                         " expression " ++ (show $ xmlExpression c)  ++ 
                         ", required "   ++ (show $ xmlRequired c)   ++ 
                         ", provided "   ++ (show $ xmlProvided c)   ++ ". ")
   else 
    Fail (unwords [showError e | e <- ts, isSuccess e == False])   
   
  
xml2Transformation :: XmlTransformation -> ParserResult GenT
xml2Transformation t = 
 let as = splitAndTrim ',' (tArgs t)
 in 
  case tName t of 
   "selectScenarios" -> Success (GenT (SelectScenarios as))

   "selectUseCases" -> Success (GenT (SelectUseCases as))
   
   "bindParameter" -> case as of
                       [x,y] -> Success (GenT (BindParameter x y))
                       otherwise -> Fail "Invalid number of arguments to the bind parameter transformation"
   
   "evaluateAspects" -> Success (GenT (EvaluateAspects as))
   
   "selectComponents" -> Success (GenT (SelectComponents as))

   "selectAndMoveComponent" -> case as of
                                [x,y] -> Success (GenT (SelectAndMoveComponent x y))
                                otherwise -> Fail "Invalid number of arguments to the select and move transformation"

   "createBuildEntries" -> Success (GenT (CreateBuildEntries as))
   
   "preprocessFiles" -> Success (GenT (PreProcessor as))

   otherwise -> Fail ("Invalid transformation: " ++ tName t)

parseConstraint :: Maybe String -> Either ParseError FeatureExpression
parseConstraint Nothing = parse pzero "" ""
parseConstraint (Just s)  = parse parseExpression "" s

