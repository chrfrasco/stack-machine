module VirtualMachine where

data Machine = Machine { instructions   :: Instructions
                       , dataStack      :: [Value] }
               deriving Show

newMachine :: Instructions -> Machine
newMachine instructions = Machine instructions []

runMachine :: Machine -> Value
runMachine (Machine (instr:instrs) dataStack) = runMachine $ Machine instrs (dispatch instr dataStack)
runMachine (Machine [] dataStack)             = head dataStack

data Instruction  = Add | Sub | Mul | Div | Push Int deriving Show
type Instructions = [Instruction]

type Value = Int
type DataStack = [Value]

dispatch :: Instruction -> DataStack -> DataStack

dispatch Add stack = binaryOperation (+)   stack
dispatch Sub stack = binaryOperation (-)   stack
dispatch Mul stack = binaryOperation (*)   stack
dispatch Div stack = binaryOperation (div) stack

dispatch (Push x) [] = [x]
dispatch (Push x) xs = x:xs

binaryOperation :: (Value -> Value -> Value) -> DataStack -> DataStack
binaryOperation operand (x:y:rest) = (operand x y) : rest
binaryOperation operand _          = error "not enough values on the stack"
