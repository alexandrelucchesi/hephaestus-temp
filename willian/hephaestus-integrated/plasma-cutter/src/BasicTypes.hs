module BasicTypes where

type Variables = [Variable]
type Variable = (VarName, Value)
type VarName = String
type Value = Bool

type Environment = (Variables, Stack)

type Stack = [Bool]

data Line = Condition String
          | EndCondition
          | CodeLine String
          deriving (Show)

data Expression = And Expression Expression
                | Or Expression Expression
                | Not Expression
                | ExpId Id
   deriving(Show)

type Id = String

