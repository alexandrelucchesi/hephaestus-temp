module HomeBanking where

import UseCaseModel.Types
import Transformations.UseCaseModel

import Test.HUnit

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
