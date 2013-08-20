\documentclass{article}

\usepackage{a4wide}

%include polycode.fmt
%include lhs2tex.sty
%options ghci -i../../src/:../../../core/src/:../../../uc-model/src/:../../../configuration-knowledge/src/:../../../feature-modeling/src/:../../../req-model/src/:../../../component-model/src/ 

%format |=> = "\Rightarrow"


\title{A Simple Module for Experimenting with Advice Compositions}

\author{Rodrigo Bonif\'{a}cio}

\begin{document}

\maketitle

\section{Introduction}

This document is a \emph{Literate Haskell} (LHS) module, proposed for 
experimenting with advice compositions. Basically, it allow us to 
understand the effect of applying the different types of advices 
(Before, After, and Around) using the Home Banking product line. 

%if False
\begin{code}
module HomeBanking where

import UseCaseModel.Types
import UseCaseModel.PrettyPrinter.Latex
import Transformations.UseCaseModel

import Test.HUnit
import BasicTypes
import Text.PrettyPrint.HughesPJ
\end{code}
%endif


\section{List of Scenarios}

The scenarios shown in Figures~\ref{} are considered in this module:
Using our approach, a scenario could be extended by some advice, if a selection of features was selected or not. 
Nevertheless, in this document we are primarly interested in testing and evaluating the 
\texttt{evalFunction}, which takes as input an advice \texttt{adv} and a 
scenario \texttt{sc}, and then returns a new scenario that is the result of composition 
between them. One interesting point about using LHS is that one could show other scenarios in 
this document, just introducing a command sucha as: 

\begin{verbatim}
\perform{let s = show (scenarioToLatex SCID) in putStrLn (formatString s)}
\end{verbatim}

This command guides the lhs2tex compile to call the GHC Interpreter and 
to evaluate the \texttt{let ...} expression. 

\begin{figure}
\perform{let s = show (scenarioToLatex sc01) in putStrLn (formatString s)}
\caption{Withdraw money transaction.}
\label{fig:sc01}
\end{figure}

\newpage

\section{List of Advice}

Next we enumerate each advice considered in this module. First, 
{\bf Advice ADV01} models the PIN authentication mechanism. It is applied 
after each step marked with the \texttt{authentication} tag.

\begin{figure}[htb]
 \perform{let s = show (adviceToLatex adv01) in putStrLn (formatString s)}
 \caption{PIN authentication advice.}
 \label{fig:adv01}
\end{figure}

Therefore, calling \texttt{evalFunction adv01 sc01} leads to the 
scenario shown in Figure~\ref{fig:eval-adv01}. 

\begin{figure}[htb]
 \perform{let s = show (flowToLatex $ steps (evaluateAdvice adv01 sc01)) in putStrLn (formatString s)}
 \caption{The effect of applying \texttt{evaluateAdvice adv01 sc01}.}
 \label{fig:eval-adv01}
\end{figure}

\newpage

Note that the evaluation of the PIN advice, regarding the first scenario, just introduces 
the steps related to the PIN authentication, after any step marked with the ``authentication'' 
tag. Consider now a different advice, which starts a transaction before any step marked with 
the ``transaction'' tag, then it returns to the flow of events of the scenario, and finally commit a transaction. 
In order to model an extension such as that, we design the \emph{Around Advice} depicted in  
Figure~\ref{fig:adv02}.

\begin{figure}[htb]
 \perform{let s = show (adviceToLatex adv02) in putStrLn (formatString s)}
 \caption{Around Advice for the transaction concern. After the first step, it should \emph{Proceed} with the scenario flow and 
  then perform the remaining two steps (adv0202, adv0203).}
 \label{fig:adv02}
\end{figure}

\newpage

Figure~\ref{fig:eval-adv02} shows the effect of evaluating the Transaction concern advice with respect to the 
withdraw scenario. 

\begin{figure}[htb]
 \perform{let s = show (flowToLatex $ steps (evaluateAdvice adv02 sc01)) in putStrLn (formatString s)}
 \caption{The effect of applying \texttt{evaluateAdvice adv02 sc01}.}
 \label{fig:eval-adv02}
\end{figure}

So, just to understand the effect of the PROCEED construct. Suppose that a specifier forgot to 
introduce the PROCEED clause in the advice of Figure~\ref{fig:adv02}. The result of applying 
this \emph{wrong} asset to the withdraw scenario is depicted in Figure~\ref{fig:eval-adv03}. Notice 
that steps \texttt{sc0104} and \texttt{sc0105} are not present in the resulting scenario. This occurs 
because, without the PROCEED construct, all steps of the scenario that follows a specific joinpoint 
are ignored.  

\begin{figure}[htb]
 \perform{let s = show (flowToLatex $ steps (evaluateAdvice adv03 sc01)) in putStrLn (formatString s)}
 \caption{The effect of applying \texttt{evaluateAdvice adv03 sc01}.}
 \label{fig:eval-adv03}
\end{figure}

\newpage 

\section{The input code}
\begin{code}


sc01 :: Scenario
sc01 = Scenario "sc01"
                "This scenario allows a customer to withdraw money from a previously selected account"   
                [IdRef "start"]
                [sc0101, sc0102, sc0103, sc0104, sc0105]
                [IdRef "end"]

adv01 :: Advice
adv01 = Advice After "adv01" "desc" [AnnotationRef "authentication"] [adv0101, adv0102]

adv02 :: Advice
adv02 = Advice Around "adv02" "desc" [AnnotationRef "transaction"] [adv0201, adv02P, adv0202, adv0203]

adv03 :: Advice
adv03 = Advice Around "adv03" "desc"  [AnnotationRef "transaction"] [adv0201, adv0202, adv0203]

sc0101 :: Step
sc0101 = Step "sc0101" 
              "The customer selects the withdraw option." 
              "-" 
              "The system creates a new withdrawal and asks for the amount to withdraw."
              []

sc0102 :: Step
sc0102 = Step "sc0102" 
              "The customer fills in the amount to withdraw."
              "-" 
              "The system retrieves the current balance of the selected account." 
              []

sc0103 :: Step
sc0103 = Step "sc0103"
              "-" 
              "-"
              "The system verifies that the requested amount is not greater than current balance plus {limit}"
              ["authentication"]

sc0104 :: Step
sc0104 = Step "sc0104"
              "-" 
              "-"
              "The bank system withdraws the amount from the account."
              ["transaction"]

sc0105 :: Step
sc0105 = Step "sc0105"
              "-" 
              "-"
              "The cash money is provided to the customer."
              []


adv0101 :: Step
adv0101 = Step "adv0101"
                "-" 
                "- "
                "The system asks the customer to enter her personal identification number."
                []

adv0102 :: Step
adv0102 = Step "adv0102"
                "The customer fills in the personal identification number." 
                "- "
                "The system authenticates the customer's personal identification number."
                []


adv0201 :: Step
adv0201 = Step "adv0201"
               "-"
               "-"
               "The transaction handler starts the processing of a transaction."
               []

adv02P :: Step
adv02P = Proceed

adv0202 :: Step
adv0202 = Step "adv0202"
               "-"
               "-"
               "An entry with the transaction information is logged to the overview of the completed transactions of the customers account."
               []
adv0203 :: Step 
adv0203 = Step "adv0203"
                "-"
                "-"
                "The transaction is removed from the transaction queue."
                []

test1 = TestCase (assertEqual "expected ids before weaving" ["sc0101","sc0102","sc0103","sc0104","sc0105"]  [sId s | s <- steps sc01] )

test2 = TestCase (assertEqual "expected ids after weaving" 
                              ["sc0101"
                              ,"sc0102"
                              ,"sc0103"
                              ,"adv0101"
                              ,"adv0102"
                              ,"sc0104"
                              ,"sc0105"] [sId s | s <- steps (evaluateAdvice adv01 sc01)] )

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

formatString :: String -> String
formatString = removeCifrao . removeCR . removeBLCommand 

removeCifrao :: String -> String 
removeCifrao s = 
 let st = words s
 in unwords [if c == "U$" then "U\\$"  else c | c <- st]

removeCR :: String -> String
removeCR s =  [if c == '\n' then ' ' else c | c <- s]

removeBLCommand :: String -> String
removeBLCommand s = replaceString "\\bl" "\\\\ \\hline" s

\end{code}




\end{document}