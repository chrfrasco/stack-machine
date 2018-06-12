module VirtualMachineTest where

import qualified Test

import VirtualMachine hiding (run, new)
import qualified VirtualMachine as VM

t = VM.run . VM.new

tests = [ ("push",                   t [(Push 1)],                Right $ Just 1)
        , ("addition",               t [(Push 2), (Push 2), Add], Right $ Just 4)
        , ("subtraction",            t [(Push 2), (Push 2), Sub], Right $ Just 0)
        , ("multiplication",         t [(Push 2), (Push 2), Mul], Right $ Just 4)
        , ("division",               t [(Push 2), (Push 2), Div], Right $ Just 1)
        , ("empty instruction list", t [],                        Right Nothing)
        , ("too few ops on stack",   t [(Push 2), Div],           Left "not enough values on the stack") ]

main :: IO ()
main = Test.run tests

