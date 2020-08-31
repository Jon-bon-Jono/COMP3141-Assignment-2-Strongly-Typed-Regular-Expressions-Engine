# COMP3141-Assignment-2-Strongly-Typed-Regular-Expressions-Engine
Implementation of a strongly typed regular expressions engine using GADTs and a number of additional convenience combinators for working with regular expressions in Haskell. Intended to give experience with Haskell type system extensions (specifically GADTs), programming with monads and applicative functors.

- Hare.hs: implementation of the regex engine and combinators
- Untyped.hs: untyped, unextended version of the regex engine
- HareMonad.hs: contains the monadic type used to write the matching algorithm
- Tests.hs: contains the main function for running tests
- Tests/Support.hs: support code for running tests
- Tests/UnitTests.hs: properties for the basic regular expressions
- Tests/Transcript.hs: acceptance tests for your combinators, for analysing a UNSW transcript
- Tests/Examples.hs: all examples from the spec, as unit tests.

## Combinators:
cons function
- `cons :: RE a -> RE [a] -> RE [a]`
- `"10100" =~ cons (Char ['1']) (Star (Char ['0'])) :: [String]["10","1","100","10","1"]`
   - `["10","1","100","10","1"]`
- `"10100" =~ cons (Char ['1']) (Action (const []) Empty) :: [String]["1","1"]`
   - `["1","1"]`

plus function
- `plus :: RE a -> RE [a]`
- `"10100" =~ plus (Char ['0']) :: [String]["0","00","0","0"]`
   - `["0","00","0","0"]`
- `let atoz = Char ['a'..'z']`
   - `let digits = Char ['0'..'9']`
   - `"ab1c3" =~ plus (atoz `Seq` digits) :: [[(Char,Char)]]
      - `[[('b','1'),('c','3')],[('b','1')],[('c','3')]]`

string function
- `string :: String -> RE String`
- `let comp3141 = string "COMP3141"`
`"My favourite subject is COMP3141" =~ comp3141 :: Maybe String`
      - `Just "COMP3141"`
   - `"My favourite subject is MATH1141" =~ comp3141 :: Maybe String`
      - `Nothing`
