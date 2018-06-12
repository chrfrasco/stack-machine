module VirtualMachine (new, run, Result, Instruction(..)) where

data Machine a = Machine { instrs :: Instructions a
                         , stack  :: DataStack a } deriving Show

data Instruction a  = Add | Sub | Mul | Div | Push a deriving Show
type Instructions a = [Instruction a]

type DataStack a = [a]

type Result a = Either String a

-- construct a new machine
new :: (Fractional a) => Instructions a -> Machine a
new instructions = Machine instructions []

-- evaluate a machine instance
run :: (Fractional a) => Machine a -> Result (Maybe a)

run (Machine (instr:instrs) stack) = 
  case dispatch instr stack of
    Right newStack -> run (Machine instrs newStack)
    Left err       -> Left err

run (Machine [] stack) = Right (safeHead stack)
  where safeHead []    = Nothing
        safeHead (x:_) = Just x


-- dispatch some action to the machiine
dispatch :: (Fractional a) => Instruction a -> DataStack a -> Result (DataStack a)

dispatch Add stack = binOp (+) stack
dispatch Sub stack = binOp (-) stack
dispatch Mul stack = binOp (*) stack
dispatch Div stack = binOp (/) stack

dispatch (Push x) [] = Right [x]
dispatch (Push x) xs = Right $ x:xs

-- abstraction over 2-arity operation
binOp :: (Fractional a) => (a -> a -> a) -> DataStack a -> Result (DataStack a)
binOp op (x:y:rest) = Right $ (op x y) : rest
binOp op _          = Left "not enough values on the stack"

