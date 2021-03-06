-- | This module provides functionality to non-deterministically match certain
-- criteria on lists. These may involve order, consecutivity, amount or
-- arbitrary predicates. Matchers may also be composed to build more powerful
-- matchers on already matched results or even on unmatched results.
module Matcher
    -- * Basics
    ( Match
    , Matcher
    , runMatcher
    , firstMatchOf
    -- * Basic matchers
    , atLeast
    , atMost
    , lastN
    , firstN
    , dropWhile
    , takeWhile
    , consecutiveBy
    , setConsecutiveBy
    , splitBy
    , filter
    -- * Combine matchers
    , before
    , onRestOf
    , onMatchOf
    , (>->)
    ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Tuple
import           Prelude             hiding (dropWhile, takeWhile, filter)
import qualified Data.List           as L

-- | Groups all adjacent-successive elements together, projected by a function.
groupSuccBy :: Enum b => (a -> b) -> [a] -> [[a]]
groupSuccBy f [] = []
groupSuccBy f (x : xs) = go x [] xs
  where
    go x succs []                          = [reverse $ x : succs]
    go x succs (x' : xs) | successive x x' = go x' (x : succs) xs
                         | otherwise       = reverse (x : succs) : go x' [] xs

    prInt           = fromEnum . f
    successive x x' = prInt x + 1 == prInt x'

type Match a = ([a], [a])
--               ^    ^
--               |    '-- Remaining unmatched input
--               |
--               '------- Already matched input

-- Every matcher should fulfill the following guarantee:
--     For every xs: when (ys, r) = f xs, then r = xs \ ys
--
newtype Matcher a = M ([a] -> [Match a])
--                      ^     ^
--                      |     '-- Non-determinisitc
--                      |
--                      '-------- Input values to be matched

-- | Adds an empty rest (helper function for matchers which match all input).
noRest :: [a] -> Match a
noRest xs = (xs, [])

instance Semigroup (Matcher a) where
    (<>) = before

instance Monoid (Matcher a) where
    mempty  = M $ const []

-- | Run a 'Matcher' on an input, and return its first match, if any.
--runMatcher :: Matcher a -> [a] -> ([a], [a])
--runMatcher (M f) = fromMaybe ([], []) . listToMaybe . f

runMatcher :: Matcher a -> [a] -> [Match a]
runMatcher (M f) = f

-- | As 'Matcher' lacks an instance of 'Applicative' and consequently of
-- 'Alternative' the following less powerful version is provided.
firstMatchOf :: [(Matcher a, b)] -> [a] -> Maybe (b, [Match a])
firstMatchOf ms xs = foldr (\t r -> f t <|> r) Nothing ms
  where
    f (m, y) = case runMatcher m xs of
        [] -> Nothing
        ms -> Just (y, ms)

-------------------------------------------------------------------------------
-- Combining matchers

-- | Use the second matcher on the match of the first.
(>->) :: Matcher a -> Matcher a -> Matcher a
M f >-> M g =  M $ \xs ->
    [ (zs, r ++ r')
    | (ys, r) <- f xs
    , (zs, r') <- g ys
    ]

infixl 5 >->

onMatchOf :: Matcher a -> Matcher a -> Matcher a
onMatchOf = flip (>->)

infixl 5 `onMatchOf`

-- | Merge the results of two matchers, but make the results of the first one
-- more important.
before :: Matcher a -> Matcher a -> Matcher a
before (M f) (M g) = M $ (++) <$> f <*> g

-- | Apply the first matcher to the rest of the second one.
onRestOf :: Matcher a -> Matcher a -> Matcher a
onRestOf (M f) (M g) = M $ \xs ->
    [ (ys ++ zs, r')
    | (ys, r) <- g xs
    , (zs, r') <- f r
    ]

-------------------------------------------------------------------------------
-- Basic matchers

-- | Only succeeds if there is at least a certain amount of elements in the
-- input.
atLeast :: Int -> Matcher a
atLeast n = M f
  where
    f xs = if length (take n xs) < n then [] else [noRest xs]

-- | Only succeeds if there is at most a certain amount of elements in the
-- input.
atMost :: Int -> Matcher a
atMost n = M f
  where
    f xs = if length (take (succ n) xs) > n then [] else [noRest xs]

-- | Matches up to a certain number of elements at the end of the input.
lastN :: Int -> Matcher a
lastN n = M $ \xs -> [swap $ splitAt (length xs - n) xs]

-- | Matches up to a certain number of elements at the beginning of the input.
firstN :: Int -> Matcher a
firstN n = M $ \xs -> [splitAt n xs]

-- | Discards input to the rest as long as the predicate does not fail.
dropWhile :: (a -> Bool) -> Matcher a 
dropWhile p = M $ \xs -> [swap $ span p xs]

-- | Matches input as long as the predicate does not fail.
takeWhile :: (a -> Bool) -> Matcher a
takeWhile p = M $ \xs -> [span p xs]

-- | Groups the list into successive segments and produces a match for each.
consecutiveBy :: Enum b => (a -> b) -> Matcher a
consecutiveBy f = M $ \xs -> case groupSuccBy f xs of
    []     -> []
    g : gs -> go [] g gs
  where
    go :: [[a]] -> [a] -> [[a]] -> [([a], [a])]
    go l m []         = [(m, concat $ reverse l)]
    go l m r@(x : xs) = (m, concat $ reverse l ++ r) : go (m : l) x xs

-- | In contrast to 'consecutiveBy',  reorders the input after the criteria.
setConsecutiveBy :: (Ord b, Enum b) => (a -> b) -> Matcher a
setConsecutiveBy f =
    M $ \xs -> runMatcher (consecutiveBy f) $ L.sortBy (comparing f) xs

-- | Splits the input into all possible values occuring in the input after
-- applying the translation function on it. Each chunk then gets a single match.
splitBy :: Ord b => (a -> b) -> Matcher a
splitBy f = M $ \xs -> map (`extract` xs) $ ordNub $ map f xs
  where
    extract y = L.partition ((== y) . f)
    ordNub = map head . L.group . L.sort

-- | Matches all elements which satisfy the predicate.
filter :: (a -> Bool) -> Matcher a
filter p = M $ (: []) . L.partition p
