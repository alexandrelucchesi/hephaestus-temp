\section{Graphical User Interface}

\begin{code}
module Main where

import qualified BasicTypes as Core

import Data.Maybe

import Data.List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New
import System.IO
-- import System
import qualified Data.Tree as Tree
import Control.Concurrent

import Text.XML.HXT.Core

import System.Environment
import System.Directory
import System.FilePath

import ExportProduct

import Ensemble.Types 


import RequirementModel.Types
import RequirementModel.Parsers.XML.XmlRequirementParser

import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.Parsers.XML.XmlUseCaseParser (parseUseCaseFile)
import UseCaseModel.Types

import BusinessProcess.Types

import FeatureModel.Types
import FeatureModel.Parsers.GenericParser 

import SPL.Transformations.Parsers.XML.XmlConfigurationParser

import SPL.Interpreter
import SPL.Types
import SPL.StaticChecker

import ComponentModel.Parsers.ParserComponentModel

data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String } deriving Show
data ErrorData = ErrorData { inputModel :: String, errorDesc :: String }

fmSchema :: String 
fmSchema = "schema_feature-model.rng"

rmSchema :: String 
rmSchema = "schema_requirements.rng"

ucSchema :: String
ucSchema = "schema_aspectual-use_cases-user_view.rng" 

fcSchema :: String
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String 
ckSchema = "schema-configuration-knowledge.rng"

normalizedSchema cDir sch = cDir </> sch 


data GUI = GUI {
      window :: Window, 
      ckWindow :: Window, 
      fmWindow :: Window, 
      rmFChooser  :: FileChooserButton, 
      ucmFChooser :: FileChooserButton,
      cmFChooser  :: FileChooserButton,  
      fmFChooser  :: FileChooserButton, 
      imFChooser  :: FileChooserButton, 
      ckFChooser  :: FileChooserButton,
      sdFChooser  :: FileChooserButton, 
      outFChooser :: FileChooserButton, 
      ckList     :: TreeView,
      errList    :: TreeView,
      featureTree    :: TreeView,
      fcheckerTButton  :: ToolButton,
      satTButton       :: ToolButton,
      badSmellsTButton :: ToolButton,
      weavingTButton   :: ToolButton, 
      displayFmTButton :: ToolButton,
      editCkTButton    :: ToolButton,
      checkCkTButton   :: ToolButton,
      printUCMTButton  :: ToolButton,
      printAWITButton  :: ToolButton
}

-- 
-- The entry point of our applicaton. Basically, 
-- it retrieves the GUI definition file, instantiate 
-- a GUI model and connect it to user action events. 
-- 
main :: IO ()
main = do
  unsafeInitGUIForThreadedRTS
  -- initGUI, if not using threads
  
  Just gladefile <- xmlNew "vm.glade"  
  
  gui <- loadGlade gladefile

  connectGui gui 

-- --------------------------------------------------------------------------
-- Load an instance of GUI the data type.
-- This design is based on the Real World Haskell book 
-- --------------------------------------------------------------------------
loadGlade f = 
 do
   -- retrieve the GUI windows
   w   <- xmlGetWidget f castToWindow "mainWindow"
   ckw <- xmlGetWidget f castToWindow "ckWindow"              
   fmw <- xmlGetWidget f castToWindow "fmWindow"

   -- retrieves the file chooser elements.
   rmfc  <- xmlGetWidget f castToFileChooserButton "rmFileChooser"
   ucmfc <- xmlGetWidget f castToFileChooserButton "ucmFileChooser"
   cmfc  <- xmlGetWidget f castToFileChooserButton "cmFileChooser"
   fmfc  <- xmlGetWidget f castToFileChooserButton "fmFileChooser"
   imfc  <- xmlGetWidget f castToFileChooserButton "imFileChooser"
   ckfc  <- xmlGetWidget f castToFileChooserButton "ckFileChooser"
   sdfc  <- xmlGetWidget f castToFileChooserButton "sdFileChooser"
   outfc <- xmlGetWidget f castToFileChooserButton "outputFileChooser"
   
  -- retrieves the tree view and list elements
   [ckl, errl, ftree] <- mapM (xmlGetWidget f castToTreeView) ["ckList"
                                                              , "errorList"
                                                              , "featureTree"
                                                              ] 
   -- retrieves the tool buttons
   fctb   <- xmlGetWidget f castToToolButton "cftb"
   sattb  <- xmlGetWidget f castToToolButton "sattb"
   fbstb  <- xmlGetWidget f castToToolButton "fbstb"
   swptb  <- xmlGetWidget f castToToolButton "swptb"
   dfmtb  <- xmlGetWidget f castToToolButton "dfmtb"
   ecktb  <- xmlGetWidget f castToToolButton "ecktb" 
   ccktb  <- xmlGetWidget f castToToolButton "ccktb" 
   pucmtb <- xmlGetWidget f castToToolButton "pucmtb"
   pawitb <- xmlGetWidget f castToToolButton "pawitb" 

   -- returns the GUI instance                                                                             
   return $ GUI {
                window      = w, 
                ckWindow    = ckw, 
                fmWindow    = fmw, 
                rmFChooser  = rmfc, 
                ucmFChooser = ucmfc,
                cmFChooser  = cmfc,
                fmFChooser  = fmfc,
                imFChooser  = imfc,
                ckFChooser  = ckfc,
                sdFChooser  = sdfc,
                outFChooser = outfc,
                ckList      = ckl,
                errList     = errl,
                featureTree = ftree, 
                fcheckerTButton  = fctb,
                satTButton  = sattb,
                badSmellsTButton = fbstb,
                weavingTButton = swptb, 
                displayFmTButton = dfmtb,
                editCkTButton   = ecktb,
                checkCkTButton  = ccktb, 
                printUCMTButton = pucmtb,
                printAWITButton = pawitb
              }

-- ---------------------------------------------------------
-- Connect the GUI to the user events.
-- ----------------------------------------------------------
connectGui gui = 
 do
  onDestroy (window gui) mainQuit
  onDelete  (fmWindow gui) $ \event -> widgetHide (fmWindow gui) >> return True 
  
  ckStore <- createCKStore
  New.treeViewSetModel (ckList gui) ckStore
  setupCKView (ckList gui) ckStore
 
  errorStore <- createErrorStore
  New.treeViewSetModel (errList gui) errorStore
  setupErrorView (errList gui) errorStore
  
  featureStore <- createFeatureStore 
  New.treeViewSetModel (featureTree gui) featureStore
  setupFeatureView (featureTree gui) featureStore
  
  onToolButtonClicked (weavingTButton gui)   (weaveFiles gui errorStore)
  onToolButtonClicked (fcheckerTButton gui)  (checkFiles gui errorStore)
  onToolButtonClicked (displayFmTButton gui) (displayFeatureModel gui featureStore) 
  -- onToolButtonClicked (editCkTButton gui)    (editConfigurationKnowledge gui)
  onToolButtonClicked (checkCkTButton gui) (checkConfiguration gui errorStore)
  onToolButtonClicked (printUCMTButton gui) (printUCM gui)
  onToolButtonClicked (printAWITButton gui) (printAWI gui)
  
  widgetShowAll (window gui)   
  mainGUI


-- 
-- A nice function :P for getting instances of the input models. 
-- This function deals with two causes of failures that are likely to 
-- happen: either a file was not selected or it was selected but 
-- the parser failed. In that cases, this function returns 
--   Core.Fail "message"
-- 
-- To use this function, we have to pass as argument a
-- FileChooser (fc), a parser for the specific model (parser), and a 
-- message (msg) that should be displayed when the user have not 
-- selected a file using fc.
--
-- Withouth high order functions, it could be hard (or verbose) to implement a 
--  generic solution such as that.
 
getInputModel (fc, parser, msg) = 
 do 
  f <- fileChooserGetFilename fc 
  case f of 
   (Just fn) -> do 
                 m <- (parser fn)
                 return m
   otherwise -> return (Core.Fail msg)

     
-- ----------------------------------------------------------------------------------
-- starts the weaving process (or product derivation process) 
-- when the user clicks on the "generate products" button. 
-- ----------------------------------------------------------------------------------
weaveFiles gui store = 
 do 
   listStoreClear store 

   parsedModels <- parseAllInputModels gui

   case (fst parsedModels) of 
     (Core.Success rm, Core.Success um, Core.Success cm, Core.Success fm, Core.Success im, Core.Success ck) -> 
         do    
           let fc  = FeatureConfiguration im
           let assets = Assets rm um (BPM [])
           let splAssetBase = SPLAssetBase assets cm
           let spl = SPLModel fm ck splAssetBase
           let product = build fc spl
           exportResult gui product 

     otherwise -> do 
       displayErrorsOnParsedModels (snd parsedModels) store
       showDialog  (window gui) 
                   MessageError 
                   "Errors found when parsing the input files."

-- This function returns a representation of the parsed input models. 
-- In fact, it is a tuple whose first element is indeed a 6-tuple with the
-- parsed input models wrapped as ParserResults (such as Core.Success rm). 
-- 
-- The second element is a list that could be used to retrieve both 
-- the status of a parser (Core.Success or Core.Fail) together with a 
-- proper error message, in case of failure. 
parseAllInputModels gui = do
   cDir <- getCurrentDirectory
   let ns = normalizedSchema cDir

   prm <- getInputModel ((rmFChooser  gui), parseRequirementModel (ns rmSchema), "Requirement model not selected.")
   pum <- getInputModel ((ucmFChooser gui), parseUseCaseFile (ns ucSchema), "Use case model not selected.")
   pcm <- getInputModel ((cmFChooser  gui), parseComponentModel, "Component model not selected.")
   pfm <- getInputModel ((fmFChooser  gui), parseFeatureModel' (ns fmSchema), "Feature model not selected.")
   pim <- getInputModel ((imFChooser  gui), parseInstanceModel (ns fcSchema), "Instance model not selected.")
   pck <- getInputModel ((ckFChooser  gui), parseConfigurationKnowledge (ns ckSchema), "Configuration knowledge not selected.") 
 
   let parseResults = [(Core.isSuccess prm, Core.showError prm)
                      ,(Core.isSuccess pum, Core.showError pum)
                      ,(Core.isSuccess pcm, Core.showError pcm)
                      ,(Core.isSuccess pfm, Core.showError pfm)
                      ,(Core.isSuccess pim, Core.showError pim)
                      ,(Core.isSuccess pck, Core.showError pck)
                      ]
   
   return ((prm, pum, pcm, pfm, pim, pck), parseResults)

-- ------------------------------------------------------------------------------------
-- starts the file checker process, which updates 
-- the error list if any error was found. 
-- ------------------------------------------------------------------------------------
checkFiles gui store = 
 do
   listStoreClear store
   
   parsedModels <- parseAllInputModels gui

   case (fst parsedModels) of 
     (Core.Success rm, Core.Success um, Core.Success cm, Core.Success fm, Core.Success im, Core.Success ck) -> 
         do    
           showDialog (window gui)
                      (MessageInfo)
                      "No errors found when parsing the input files."
     otherwise -> 
         do 
           displayErrorsOnParsedModels (snd parsedModels) store
           showDialog  (window gui) 
                       MessageError 
                       "Errors found when parsing the input models."

-- auxiliarly function that displays errors 
-- found when parsin the input models.
displayErrorsOnParsedModels parserResults store = do
  let models = zip ["Requirement model"
                   ,"Use case model"
                   ,"Component model"
                   ,"Feature model"
                   ,"Instance model"
                   ,"Configuration knowledge"
                   ] parserResults
  let errors = [(a,c) | (a, (b,c)) <- models, b == False]
  updateErrorStore store (map (\(x,y) -> ErrorData x y) errors)

-- Start the process of checking a configuration model.
-- The details of this function could be found in the 
-- module ConfigurationModel.StaticChecker
checkConfiguration gui store = do 
  cDir <- getCurrentDirectory
  let ns = normalizedSchema cDir
  
  pfm <- getInputModel ((fmFChooser gui), parseFeatureModel' (ns fmSchema), "Feature model not selected.")
  pck <- getInputModel ((ckFChooser gui), parseConfigurationKnowledge (ns ckSchema), "Configuration knowledge not selected.")
  pim <- getInputModel ((imFChooser  gui), parseInstanceModel (ns fcSchema), "Instance model not selected.")
  case (pfm, pim, pck) of
   (Core.Success fm, Core.Success im, Core.Success ck) -> 
      do
        let exps = ckStaticChecker fm (FeatureConfiguration im) ck
        updateErrorStore store (map (ErrorData "Configuration Knowledge") exps)
    
   otherwise -> showDialog  (window gui) 
                            MessageError 
                            "Error when parsing the input files." 
 

-- 
-- The next two functions export the use case model or the aspect interfaces to a 
-- latex representation 
-- 
printUCM gui = printUCM' gui exportUcmToLatex
printAWI gui = printUCM' gui exportAspectInterfacesToLatex

-- This function checks if the uc model was loaded and is correct.
-- Then it exports either the use case model or the list of aspect' 
-- interfaces
printUCM' gui fn = do
  cDir <- getCurrentDirectory
  let ns = normalizedSchema cDir
  pucm <- getInputModel ((ucmFChooser gui), parseUseCaseFile (ns ucSchema), "Use case model not selected.")
  printUCM'' gui fn pucm
 where 
  printUCM'' gui fn (Core.Fail _) = showDialog (window gui) MessageError "Error parsing a use case model." 
  printUCM'' gui fn (Core.Success ucm) = do
                 dialog <- fileChooserDialogNew
                           (Just "Save as...")     -- the dialog title          
                           (Just (window gui))     -- the main window         
                           FileChooserActionSave   -- the type of the dialog          
	                   [("gtk-cancel", ResponseCancel), ("gtk-save", ResponseAccept)] -- ... and finally the actions!
                 widgetShow dialog
                 response <- dialogRun dialog
                 case response of 
                   ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                                        fn fileName ucm 

                   ResponseCancel -> putStrLn "[Hephaestus] Save dialog canceled."

                   ResponseDeleteEvent -> putStrLn "[Hephaestus] Save dialog closed."
                 widgetHide dialog
   
-- -------------------------------------------------------------------------------------
-- displays the selected feature model. Note, the 
-- current implementation has a bug. It fails on the 
-- second time a feature model is displayed. It might be a 
-- gtk2HS problem. 
-- --------------------------------------------------------------------------------------
displayFeatureModel gui store = 
 do
  cDir <- getCurrentDirectory
  let ns = normalizedSchema cDir 
  
  f <- getInputModel ((fmFChooser gui), parseFeatureModel' (ns fmSchema), "Please, select a feature model file.")
  case f of  
   Core.Success fm -> 
       do  
         let t = feature2TreeNode (fmTree fm) 
         New.treeStoreClear store 
         New.treeStoreInsertTree store [] 0 t
         widgetShowAll (fmWindow gui)
   
   Core.Fail s -> 
       do showDialog (window gui) 
                     MessageError 
                     ("Error parsing feature model: " ++ s)

-----------------------------------------------------------
-- auxiliarly functions for parsing feature models
-- TODO: this version supports just fmplugin and fmide.
-- TemplateHaskell might help here, creating a family of 
-- programs that supports different parsers.
-----------------------------------------------------------

supportedFmTypes = [ ("xml", FMPlugin), (".m" , FMIde) ]

parseFeatureModel' schema fmfile = 
 let p = [snd t | t <- supportedFmTypes , (fst t) `isSuffixOf` fmfile]
 in case p of 
  [x] -> (parseFeatureModel (schema, fmfile) x)
  otherwise -> error "Error identifying the type of the feature model."

-- -------------------------------------------------------------------------
-- export the output files of a product generated from 
-- the results of a weaving process. 
-- ------------------------------------------------------------------------
exportResult gui p = 
 do
   odir <- fileChooserGetCurrentFolder (outFChooser gui)
   (Just source) <- fileChooserGetCurrentFolder (sdFChooser gui)
   case odir of 
     Just out -> do 
          tid <- forkIO (exportProduct source out p) -- we'd better to start a new thread
          print tid 

     Nothing -> showDialog (window gui) 
                MessageError 
               "Please, select an output directory."
              
--
-- Check if a xml input file (f) adheres to the definitions 
-- of the schema (s). The store is updated with the list of 
-- errors. 
--
-- executeFileChecker Nothing s m store = updateErrorStore store [(ErrorData m "Model not loaded.")]
-- executeFileChecker (Just f) s m store = do { 
--  errs <- runX ( errorMsgCollect 
--                  >>> 
--                  readDocument [(a_validate, v_0)
--                               ,(a_relax_schema, s)
--                               ,(a_issue_errors, "0")                              
--                               ] (Core.createURI f)
--                  >>>
--                  getErrorMessages
--                ) ;
--  updateErrorStore store [ErrorData m (show e) | e <- errs]
--  } 

 
-- update the error store, with a list of errors to 
-- be appendend. 
updateErrorStore s errors = do 
  listStoreClear s
  updateErrorStore' s errors
 
updateErrorStore' s [] = return ()
updateErrorStore' s (e:errs) = do
   New.listStorePrepend s e; 
   updateErrorStore' s errs


--
-- an auxiliarly function to show simple dialogs
-- args: 
--  a) the parent window
--  b) the dialog type
--  c) the dialog message
showDialog w t  m = do
  messageDialog <- messageDialogNew (Just w) [] t ButtonsClose m	
  widgetShowAll messageDialog	
  response <- dialogRun messageDialog
  widgetHide messageDialog

-- 
-- Show the dialog for specifying configuration 
-- models. 
--
openNewCKDialog :: Window -> IO () 
openNewCKDialog ckWindow = 
 do {
	 widgetShowAll ckWindow;
 }
 
--
-- initilize the error tree list view. 
-- it mainly defines the columns rendered 
-- in the list viewer and relates such a columns
-- to the error model. 
-- 
setupErrorView view model = 
 do {
  New.treeViewSetHeadersVisible view True;

  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := inputModel row ];
  New.treeViewColumnSetTitle col1 "Input model";
  New.treeViewAppendColumn view col1;

  renderer2 <- New.cellRendererTextNew;
  col2 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col2 renderer2 True;
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := errorDesc row ];
  New.treeViewColumnSetTitle col2 "Description";
  New.treeViewAppendColumn view col2;
}

--
-- initialize the ck list view.
-- similarly to the setpErrorView.
-- 
setupCKView view model = do { 
  New.treeViewSetHeadersVisible view True;

  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := expressionData row ];
  New.treeViewColumnSetTitle col1 "Feature expression";
  New.treeViewAppendColumn view col1;
 
  renderer2 <- New.cellRendererTextNew;
  col2 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col2 renderer2 True;
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (transformationData row)];
  New.treeViewColumnSetTitle col2 "Transformations";
  New.treeViewAppendColumn view col2;
}

-- 
-- 
-- 
setupFeatureView view model = do {
  New.treeViewSetHeadersVisible view True;
      
  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := (feature2cell row) ];
  New.treeViewColumnSetTitle col1 "Name";
  New.treeViewAppendColumn view col1;

  
}
 
-- initialize error and ck stores
createErrorStore = New.listStoreNew []
createCKStore = New.listStoreNew []
createFeatureStore = New.treeStoreNew []

generateFmStore fm = (New.treeStoreNew [feature2TreeNode (fmTree fm)])

feature2TreeNode :: FeatureTree -> Tree.Tree Feature
feature2TreeNode (Leaf f) = Tree.Node { Tree.rootLabel = f, Tree.subForest = [] }
feature2TreeNode (Root f cs) = Tree.Node {Tree.rootLabel = f, Tree.subForest = (map feature2TreeNode cs) }

feature2cell f = 
 let 
  fn = if (fType f == Optional) 
        then "[" ++ (fName f) ++ "]" 
        else (fName f)  
 in 
  case (groupType f) of 
   AlternativeFeature -> fn ++ " g <1-1>"  
   OrFeature -> fn ++ " g <1-*>" 
   otherwise -> fn

\end{code}

  