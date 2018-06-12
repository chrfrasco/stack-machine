module Test (run) where

import qualified Data.Either as E

type TestTriple a = (String, a, a)
type Result a = Either (TestTriple a) String

assert :: (Show a, Eq a) => String -> a -> a -> Result a
assert name got expected = if got == expected then Right name else Left (name, got, expected)


display :: (Show a) => Result a -> IO ()
display (Right name) = putStr ""
display (Left (name, expected, got)) = putStrLn message
  where message = concat [ "Test '"
                         , name
                         , "' failed: \n\tgot:    "
                         , (show got)
                         , "\n\twanted: "
                         , (show expected) ]


run :: (Show a, Eq a) => [TestTriple a] -> IO ()
run tests = do
  let results = map (uncurry3 assert) tests

  mapM_ (display) results

  let numTestsPassed = length $ filter E.isRight results in
    let numTests = length results in
      putStrLn $ concat [show numTestsPassed, "/", show numTests, " tests passed"]


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
