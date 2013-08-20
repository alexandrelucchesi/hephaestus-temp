-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.PrettyPrinter.Latex
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A Latex pretty printer for our Use Case Model data type. Useful for 
-- generating ps or pdf files from a use case model.
--
--
-----------------------------------------------------------------------------

module UseCaseModel.PrettyPrinter.Latex 
 (ckToLatex)
where 

import Text.PrettyPrint.HughesPJ

import ConfigurationKnowledge.Types

title :: String
title = "\\title{Configuration Knowledge"

ckToLatex :: ConfigurationKnowledge -> Doc
ckToLatex items = vcat [ beginDocument title 
                       , vcat $ map configurationItemToLatex items
                       , endDocument
                       ]

configurationItemsToLatex :: ConfigurationItem -> Doc
configurationItemsToLatex item = hcat [ text $ show e
				      , text " & "  
          			      , text $ show ts 
				      , text " & " 
				      , textRequired item 
				      , text " & " 
				      , textProvided item]
where  
