{-# LANGUAGE NoMonomorphismRestriction #-}
module Poker.Card
    ( deck
    , Suit(..)
    , Rank(..)
    , Card(..)
    , Hand(..)
    , HandDescr(..)
    , compareHandDescr
    , highestHand
    ) where

import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe

import Matcher             as M

data Suit
    = Spades
    | Diamonds
    | Hearts
    | Clubs
    deriving (Eq, Ord, Enum, Bounded, Show)

data Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Ord, Enum, Bounded, Show)

data Card
    = Card
    { cRank :: Rank
    , cSuit :: Suit
    } deriving (Eq, Ord, Show)

-- | A complete 52 card deck.
deck :: [Card]
deck = Card <$> enumAll <*> enumAll
  where
    enumAll = [minBound .. maxBound]

data Hand
    = HighCard      -- ^ the highest card of the hand
    | OnePair       -- ^ 2 cards with same rank
    | TwoPair       -- ^ 2 pairs
    | ThreeOfAKind  -- ^ 3 cards of same rank
    | Straight      -- ^ 5 consecutive cards
    | Flush         -- ^ 5 cards of one suit
    | FullHouse     -- ^ Triple and a pair
    | FourOfAKind   -- ^ 4 cards of same rank
    | StraightFlush -- ^ 5 consecutive cards of one suit
    | RoyalFlush    -- ^ 10 to Ace of one suit
    deriving (Eq, Ord, Show)

data HandDescr
    = HandDescr
    { hdHand  :: Hand   -- ^ The detected hand
    , hdCards :: [Card] -- ^ Cards that are part of the match
    , hdRest  :: [Card] -- ^ Unrelated cards
    } deriving (Eq, Show)

-- | Compare the description of a hand by the rules of poker. FIXME equality is wrong
compareHandDescr :: HandDescr -> HandDescr -> Ordering
compareHandDescr (HandDescr lh lcs lr) (HandDescr rh rcs rr) = case compare lh rh of
    EQ -> case lCompare lcs rcs of
        EQ -> lCompare lr rr
        o  -> o
    o  -> o
    where
    lCompare = compare `on` reverse . map cRank

consecutiveRank :: Matcher Card
consecutiveRank = consecutiveBy cRank

fromRank :: Rank -> Matcher Card
fromRank rank = M.dropWhile $ (< rank) . cRank

-- | Split all matches in 4 parts by suit.
splitBySuit :: Matcher Card
splitBySuit = splitBy cSuit

splitByRank :: Matcher Card
splitByRank = splitBy cRank

toHandDescr :: (Hand, [Match Card]) -> HandDescr
toHandDescr (h, ms) = uncurry (HandDescr h) $ head ms

highestHand :: [Card] -> Maybe HandDescr
highestHand hand = toHandDescr <$> firstMatchOf handMatchers (sort hand)
  where
    handMatchers    = [ (royalFlushM, RoyalFlush)
                      , (straightFlushM, StraightFlush)
                      , (fourOfAKindM, FourOfAKind)
                      , (fullHouseM, FullHouse)
                      , (flushM, Flush)
                      , (straightM, Straight)
                      , (threeOfAKindM, ThreeOfAKind)
                      , (twoPairM, TwoPair)
                      , (onePairM, OnePair)
                      , (highCardM, HighCard)
                      ]

    royalFlushM     = splitBySuit >-> consecutiveRank >-> fromRank Ten >-> atLeast 5
    straightFlushM  = splitBySuit >-> straightM
    fourOfAKindM    = splitByRank >-> atLeast 4
    fullHouseM      = onePairM `onRestOf` threeOfAKindM

    flushM          = splitBySuit >-> atLeast 5 >-> lastN 5
    straightM       = (consecutiveRank >-> atLeast 5 >-> lastN 5) `before` straightAceM
    threeOfAKindM   = splitByRank >-> atLeast 3
    twoPairM        = onePairM `onRestOf` onePairM
    onePairM        = splitByRank >-> atLeast 2
    highCardM       = lastN 1

    -- Special rule for straight with Ace as One
    straightAceM    = (M.filter ((== Ace) . cRank) >-> atLeast 1)
                      `onRestOf`
                      (consecutiveRank >-> fromRank Two >-> atLeast 4 >-> firstN 4)
