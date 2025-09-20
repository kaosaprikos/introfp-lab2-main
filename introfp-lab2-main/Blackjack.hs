{- |
Module      : Lab2
Description : Skeleton for lab 2: Blackjack
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Alba Mori Wallin, Harsimrat Kour, Amanda Juarez andino
Lab group   : <group 22>
-}

module Blackjack where

-- Import necessary modules
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import Data.List

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
----------------------- ----------------------------------------------------------

-- Task B1 --

-- here we are defining all the cards that exist in a fullDeck, meaning all 52 cards. But instead of listing
-- every single card we try to make shorter better code with the help of list comprehentions that still defines all the cards in a full deck. 
fullDeck :: Deck
fullDeck = [(Card r s) | r <- [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace], s <- [Hearts, Spades, Diamonds, Clubs]]


-- checks wether or not we actually have the correct amount of cards or not. if we have all 52 cards
-- bool will let us know by returning True in the terminal. If not we get False. 
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2 --

-- here we are using the function draw to be able to draw one card out of a deck and and putting it into the hand.
-- we also define error to show that it is not possible to draw a card from an empty deck. 
-- the output is only the tail of the original deck/list, because de first item of the list has now been sent into the hand.
draw :: Deck -> Hand -> (Deck, Hand)
draw [] h = error "draw: The deck is empty." 
draw (x:xs) h = (xs, x : h)


-- Task B3 --

-- in playBank we let the bank play the first hand is always empty.
playBank :: Deck -> Hand
playBank d = playBank' d []


-- here we declare that given a deck and a hand we return a hand, but we can't use the same function
-- playBank which is why we namned the function differently. We return bankhand if the deck is either
-- empty or if the value of the hand is equal or more than 16 points. Otherwise we do playBank' again with the new deck (one less card) and the new
-- bankhand (with one more card from deck) which is done with the help of draw from where. 

playBank' :: Deck -> Hand -> Hand
playBank' d bh
 | d == [] || value bh >= 16 = bh 
 | otherwise = playBank' d' bh'
    where (d', bh') = draw d bh


-- Task B4 --

-- here, we convert the length of the deck into doubles by using the function fromIntegral so that we can mulitply
-- the doubles together without syntax problems. Then with the help of the floor function we round off
-- the decimals to the integer below, all this in the function e to avoid unecessary long code. If the last number is picked we do -1
-- since lists start by the number 0 then the last number doesn't have a responding card. Therefore we
-- If not, then it just gives us the responding card of the double we sent in.
pick :: Double -> Deck -> Card  
pick double deck = if e deck == length deck then deck !! (e deck - 1) else deck !! e deck
  where 
    e deck = floor(fromIntegral (length deck) * double)

-- shuffle takes in a list of doubles and a deck.
-- if the deck is it returns an empty deck, because the deck can run out of cards eventually. 
-- but the shuffle function uses the pick function and the first double of the list to takes out a card from the deck, and then does the
-- shuffle function again but with the rest of the doubles and a deck specifically without the card that
-- we picked out. 

shuffle :: [Double] -> Deck -> Deck
shuffle _ []     = []
shuffle (y:ys) d = [pick y d] ++ shuffle ys (delete (pick y d) d)        --[x | x <- d, x /= pick y d]

runShuffle :: IO Deck
runShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

-- Task B5 --

--takes in a card and a deck and checks if the card belongs to the deck 
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

--takes in card, deck and double list. Checks if the card belongs to de original deck and if so also the shuffled deck
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) = 
  card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

--checks size of original deck and new shuffled deck
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = length (shuffle randomlist deck) == length deck

-- Task B6 --
--implementation wrapps up all of our functions and make them usable together
--main runs the game with the help of som extra code in run game and implementation
main :: IO ()
main = runGame implementation

implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

-- follow the instructions on Canvas

