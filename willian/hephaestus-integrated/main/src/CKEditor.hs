module Main where 

-- GTK / Glade packages
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.SourceView

-- 
-- Just a data type for handling widgets.
--
data GUI = GUI {
      ckEditorWindow :: Window, 
      ckTextView :: TextView
}

main :: IO()
main = do 
  initGUI
  
  Just gladefile <- xmlNew "ck.glade"  
  
  gui <- loadGlade gladefile

  connectGui gui 

loadGlade f = 
  do
   ckw <- xmlGetWidget f castToWindow "ckEditorWindow"
   ckt <- xmlGetWidget f castToTextView "ckSourceView"

   return $ GUI {
     ckEditorWindow = ckw,             
     ckTextView = ckt 
  }

connectGui gui = 
  do 
   onDestroy (ckEditorWindow gui) mainQuit
   widgetShowAll (ckEditorWindow gui)   
   mainGUI
