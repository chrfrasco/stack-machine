module VirtualMachineTest where

import VirtualMachine

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

assert :: (Show a, Eq a) => a -> a -> String -> String
assert got expected name = 
  if got == expected then
    name ++ " ok"
  else
    name ++ " failed: " ++ (show got) ++ " != " ++ (show expected)

makeAndRun = runMachine . newMachine

testCases :: [(Int, Int, String)]
testCases = [(makeAndRun [(Push 1)], 1, "push")
            ,(makeAndRun [(Push 2), (Push 2), Add], 4, "addition")
            ,(makeAndRun [(Push 2), (Push 2), Sub], 0, "subtraction")
            ,(makeAndRun [(Push 2), (Push 2), Mul], 4, "multiplication")
            ,(makeAndRun [(Push 2), (Push 2), Div], 1, "division")]

main :: IO ()
main = mapM_ putStrLn $ map (uncurry3 assert) testCases
