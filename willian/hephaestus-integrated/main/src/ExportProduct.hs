module ExportProduct (exportProduct, exportUcmToLatex, exportAspectInterfacesToLatex, exportSourceCode) where

import System.IO
-- import System
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Control.Monad
import Control.Exception

import Ensemble.Types
import ComponentModel.Types
import SPL.Types
import FeatureModel.Types
import UseCaseModel.Types
import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.PrettyPrinter.LatexUserActions
import UseCaseModel.PrettyPrinter.XML

import PlasmaCutter

import qualified BasicTypes as Core

exportProduct :: FilePath -> FilePath -> InstanceModel -> IO ()
exportProduct s o p = 
 let 
  ucmodel = ucModel $ instanceAssetBase p 
  cmodel = components $ buildData $ instanceAssetBase p
 in do
  print "\n Use cases... " 
  print $ ucmodel

  exportUcmToLatex (o ++ "/use-cases.tex") (ucmodel)
  exportUcmToLatexUserActions (o ++ "/simplified-use-cases.tex") (ucmodel)
  exportUcmToXML (o ++ "/use-cases.xml") (ucmodel)

  print "\n Copying source files to output directory \n"
  print $ map (++ "\n") [fst c | c <- cmodel]
  
  exportSourceCode s o p

exportSourceCode :: FilePath -> FilePath -> InstanceModel -> IO ()
exportSourceCode s o p = 
 let bd = buildData $ instanceAssetBase p
 in do
  copySourceFiles s o (components bd) (flattenIM p, [True])
  exportBuildFile  (o ++ "/build.lst") (buildEntries bd)
  preprocessFiles (o ++ "/build.lst") (preProcessFiles bd) o


preprocessFiles :: String -> [String] -> String -> IO()
preprocessFiles _ [] _ = return()
preprocessFiles flags files outputFolder = do 
   pid <- runCommand ("java -jar preprocessor.jar \"" ++ flags ++ "\"" ++ (concatPaths files outputFolder))
   waitForProcess pid >>= exitWith
   
concatPaths :: [String] -> String -> String
concatPaths [] _ = ""
concatPaths (str:strs) outputFolder = " \"" ++ outputFolder </> str ++ "\"" ++ (concatPaths strs outputFolder)

-- -----------------------------------------------------------------------
-- Exports a use case model as a latex document.
-- -----------------------------------------------------------------------
exportUcmToLatex f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToLatex ucm)))

exportUcmToLatexUserActions f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToLatexUserActions ucm)))

exportUcmToXML f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToXML ucm)))

exportAspectInterfacesToLatex f ucm = 
  bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (aspectInterfacesToLatex ucm)))
-- ----------------------------------------------------------------------
-- Copy selected source files to the output directory
-- ----------------------------------------------------------------------
copySourceFiles source out [] _ = return ()
copySourceFiles source out (c:cs) env = 
 do 
  createOutDir out c 
  copySourceFile source out c env
  copySourceFiles source out cs env

-- --------------------------------------------------------------------
-- Exports the list of build entries as a build file. The format of this 
-- build file should describe, for instance, which Eclipse plugins 
-- have to be considered when building an application. 
-- -------------------------------------------------------------------
exportBuildFile f es = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (concat [e ++ "\n" | e <- es]))

createOutDir out c =
 do
  print $ "Selected output dir: " ++ out
  print $ "Selected output component" ++ (snd c)
  let new = out </> (snd c)
  let newDir = dropFileName new
  print new 
  print newDir 
  createDirectoryIfMissing True newDir

-- copy a component from source directory to the out directory. 
-- Note that a component is a pair of ids (Id, Id), indicating the 
-- names of the input and output files. Usually they have the same 
-- name. 
copySourceFile source out c env = 
 do 
  let old = source </> (fst c)
  let new = out </> (snd c)
  isFile <- doesFileExist old
  isDirectory <- doesDirectoryExist old
  if isFile then do
   codeLines <- getLinesFromFile old
   putStrLn "this code"
   print codeLines 
   putStrLn "end"
   putStrLn "env"
   print $ fst env
   let newCodeLines = extractCode codeLines env
   writeLinesToFile new newCodeLines
  else if isDirectory then do
   contents <- getDirectoryContents old
   let contents' = normalizeContents contents
   let newSource = source </> (fst c)
   copySourceFiles newSource new contents' env
  else do
   undefined
  
normalizeContents :: [String] -> [(String,String)]
normalizeContents [] = []
normalizeContents (".":xs) = normalizeContents xs
normalizeContents ("..":xs) = normalizeContents xs
normalizeContents (x:xs) = [(x,x)] ++ normalizeContents xs

getLinesFromFile :: String -> IO [String]
getLinesFromFile path = do  
	handle <- openFile path ReadMode  
	contents <- hGetContents handle  
	return $ lines contents

writeLinesToFile :: FilePath -> [String] -> IO ()
writeLinesToFile path codeLines = do  
	handler <- openFile path WriteMode  
	printLinesToHandler handler codeLines

printLinesToHandler :: Handle -> [String] -> IO ()
printLinesToHandler _ [] = return ()
printLinesToHandler handler (x:xs) = do
	hPutStrLn handler x
	printLinesToHandler handler xs

flattenIM :: InstanceModel -> [String]
flattenIM im = flat [fcTree $ fc im]

flat :: [FeatureTree] -> [String]
flat [] = []
flat [(Leaf f)] = [fName f]
flat [(Root f ft)] = [fName f] ++ flat ft
flat ((Root f ft):xs) = [fName f] ++ flat ft ++ flat xs