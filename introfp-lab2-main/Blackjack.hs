{- |
Module      : Lab2
Description : Skeleton for lab 2: Blackjack
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <list your names here>
Lab group   : <group number>
-}

module Blackjack where

-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Task A1 --

-- Card examples
aCard1 :: Card
aCard1 = Card (Numeric 7) Clubs

aCard2 :: Card
aCard2 = Card Ace Diamonds

-- Hand examples, takes cards from our Card examples. 
aHand :: Hand
aHand = (aCard1 : (aCard2 : []))

-- Task A1 --

hand2 :: Hand
hand2 = [Card (Numeric 5) Hearts, Card Jack Spades, Card King Hearts]

hand3 :: Hand
hand3 = [Card King Hearts, Card (Numeric 3) Clubs, Card (Numeric 7) Diamonds]

-- size hand2
-- = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
-- = size [Card (Numeric 2) Hearts, Card Jack Spades]
-- = 1 + size [Card Jack of Spades]
-- = 1 + (1 + size [])
-- = 1 + (1 + 0)
-- = 2

-- Returns list where every element is a different step of the function size using aHand
sizeSteps :: [Int]
sizeSteps = 
            [ size aHand
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + (size [])
            , 1 + 1 + 0
            , 1 + 1
            , 2
            ]
            
-- Task A2 --

-- displayCard :: Card -> String
-- displayCard (Card x Diamonds) = show x ++ "diamonds"


-- displays cards rank and suit as strings
display :: Hand -> String
display [] = ""
display ((Card (Numeric r) s):xs) = show r ++ " of " ++ show s ++ ", " ++ display xs
display ((Card r s):xs)  = show r ++ " of " ++ show s ++ ", " ++ display xs




-- Task A3 --

-- Adds the value of each card in a hand and sums them up
valueHand :: Hand -> Int 
valueHand [] = 0
valueHand ((Card (Numeric x) _):xs) = x + valueHand xs
valueHand ((Card Ace _):xs) = 11 + valueHand xs
valueHand ((Card x _):xs) = 10 + valueHand xs

--Checks if hand value is bigger than 21 and then subtracts 10 points for each ace (changes ace value from 11 to 1)
value :: Hand -> Int
value xs = if valueHand xs > 21 then valueHand xs - sum[10 | Card Ace x <- xs] else valueHand xs
 

-- Task A4 --

--checks if the player is bust, has more than 21 points
gameOver :: Hand -> Bool
gameOver xs = value xs > 21

--decides if Guest or Bank wins depending om their final value and who has gone bust
winner :: Hand -> Hand -> Player
winner g b 
 | value b > 21 && value g < 21 = Guest 
 | value g > value b && value g <= 21 = Guest
 | otherwise = Bank

--------------------------------------------------------------------------------
-- Part B
---------------------------------------------------------------------------------

-- Task B1 --

fullDeck :: Deck
fullDeck = [(Card r s) | r <- [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace], s <- [Hearts, Spades, Diamonds, Clubs]]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

hand4 :: Hand
hand4 = []

draw :: Deck -> Hand -> (Deck, Hand)
draw [] h = error "draw: The deck is empty." 
draw (x:xs) h = (xs, x : h)
draw d h = (d, (d !! 0) : h)

-- Task B3 --

playBank :: Deck -> Hand
playBank = undefined

-- Task B4 --

pick :: Double -> Deck -> Card
pick = undefined

shuffle :: [Double] -> Deck -> Deck
shuffle = undefined

runShuffle :: IO Deck
runShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

-- Task B5 --

belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) = 
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = undefined

-- Task B6 --

-- follow the instructions on Canvas

