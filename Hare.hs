--By Jonathan Williams (z5162987) August 2020
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 
import Data.Maybe

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char cs) = do
    x <- readCharacter -- x: :: Char
    guard(x `elem` cs) -- cs :: [Char]
    pure x 
match (Seq a b) = do
    ra <- match a 
    rb <- match b  
    pure (ra,rb)
match (Choose a b) =
        match a
    <|> match b
match (Star a) = -- Hare f [Char]
        (:) <$> match a <*> (match (Star a))
    <|> pure []
match (Action f ma) = do
    a <- match ma
    let b = f a
    pure b

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (\(u,v) -> u:v) (x `Seq` xs)

infixr `append`
append :: RE [a] -> RE [a] -> RE [a]
append xs ys = Action (\(u,v) -> u ++ v) (xs `Seq` ys)

string :: String -> RE String
string [] = Action (const []) Empty
string (x:xs) = (Char [x]) `cons` (string xs)

rpt :: Int -> RE a -> RE [a]
rpt 0 re = Action (const []) Empty
rpt n re = re `cons` (rpt (n-1) re)


rptRange :: (Int, Int) -> RE a -> RE [a]
--rptRange = error "rptRange not implemented"
rptRange (x,y) (Char c) = choose (reList (x,y) c)
  where
    reList :: (Int, Int) -> RE a -> [RE a]
    reList (x,y) (Char c) = (string (replicate c)):(reList (x,y-1) 

rptRange (x,y) re 
          | x > y = (Action (const []) Empty)
          | otherwise = (rpt y re) `append` (rptRange (x,y-1) re) -- RE [a]

option :: RE a -> RE (Maybe a)
option re = (Action (\x-> Just x) re) `Choose` (Action (\x-> Nothing) Empty)

plus :: RE a -> RE [a]
plus (Char c) = (Char c) `cons` (Star (Char c)) 
plus (Seq r1 r2) = (Seq r1 r2) `cons` (Star (Seq r1 r2))

choose :: [RE a] -> RE a
choose [] = Fail
choose [x] = x
choose (x:xs) = x `Choose` (choose xs) 

atoz = Char ['a'..'z']
digits = Char ['0'..'9']
comp3141 = string "COMP3141"
re = choose [string "COMP", string "MATH", string "PHYS"]
sign = Action (fromMaybe '+') (option (Char ['-']))
programs = choose [string "COMP", string "PHYS", string "MATH"]
courseCode = programs `Seq` rpt 4 digits