module VirtualMachine where

import qualified Data.Array as A

data Machine = Machine { instructions   :: Instructions
                       , dataStack      :: [Value] }
               deriving Show

newMachine :: Instructions -> Machine
newMachine instructions = Machine instructions []

runMachine :: Machine -> Value
runMachine (Machine [] dataStack)             = head dataStack
runMachine (Machine (instr:instrs) dataStack) = runMachine $ Machine instrs (dispatch instr dataStack)

-- Instructions
data Instruction  = Add | Sub | Push Int deriving Show
type Instructions = [Instruction]

-- Data stack
type Value = Int
type DataStack = [Value]

-- Dispatch actions
dispatch :: Instruction -> DataStack -> DataStack

-- Add
dispatch Add (x:y:rest) = (x + y) : rest
dispatch Add _          = error "not enough values on stack"

-- Sub
dispatch Sub (x:y:rest) = (x - y) : rest
dispatch Sub _          = error "not enough values on stack"

-- Push
dispatch (Push x) [] = [x]
dispatch (Push x) xs = x:xs
