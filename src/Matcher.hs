-- | A list pattern matcher, which allows extraction of patterns in order,
-- matching on already matched input and on not matched input, in a
-- non-deterministic fashion.
--
-- The order of input is always preserved and sometimes even required.
module Matcher
    ( Match
    , Matcher
    , runMatcher
    , firstMatchOf
    -- * Combinators on matchers.
    , before
    , onRestOf
    , onMatchOf
    , (>->)
    , atLeast
    , atMost
    , lastN
    , firstN
    , dropWhile
    , takeWhile
    , consecutiveBy
    , splitBy
    , filter
    ) where

import Control.Applicative
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Prelude hiding (dropWhile, takeWhile, filter)

-- | Groups all adjacent-successive elements together.
groupSucc :: Enum b => (a -> b) -> [a] -> [[a]]
groupSucc _ []       = []
groupSucc f (y : ys) = go y [] ys
  where
    go x succs []                          = [reverse $ x : succs]
    go x succs (x' : xs) | successive x x' = go x' (x : succs) xs
                         | otherwise       = reverse (x : succs) : go x' [] xs

    prInt                                  = fromEnum . f
    successive x x'                        = prInt x + 1 == prInt x'

-- Every matcher should fulfill the following guarantees:
--     - For every xs: when (ys, r) = f xs, then r = xs \ ys
--
-- Matchers can do several things:
--     - Match input on different ways (non-deterministic) but the not matched
--       input has to be put to the rest
--     - Discard a complete branch
--     - Match on the rest of other matchers
--     - Match based on order
--
newtype Matcher a = M ([a] -> [([a], [a])])
--                      ^        ^    ^
--                      |        |    '---- Remaining input
--                      |        |
--                      |        '-- Already matched input
--                      |
--                     input values to be matched

-- | Adds an empty rest (helper function for matchers which match all input).
noRest :: [a] -> ([a], [a])
noRest xs = (xs, [])

instance Monoid (Matcher a) where
    mappend = before
    mempty  = M $ const []

type Match a = ([a], [a])

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
        r  -> Just (y, r)

-------------------------------------------------------------------------------
-- Combining matchers

-- | Use the second matcher on the match of the first.
onMatchOf :: Matcher a -> Matcher a -> Matcher a
onMatchOf (M f) (M g) = M $ \xs ->
    [ (zs, r ++ r')
    | (ys, r) <- f xs
    , (zs, r') <- g ys
    ]

infixl 5 `onMatchOf`

(>->) :: Matcher a -> Matcher a -> Matcher a
(>->) = onMatchOf

infixl 5 >->

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

-- | Matches only lists with at least a certain amount of elements.
atLeast :: Int -> Matcher a
atLeast n = M f
  where
    f xs = if length (take n xs) < n then [] else [noRest xs]

-- | Matches only lists with at most a certain amount of elements.
atMost :: Int -> Matcher a
atMost n = M f
  where
    f xs = if length (take (succ n) xs) > n then [] else [noRest xs]

-- | Matches the upper end of a partial match.
lastN :: Int -> Matcher a
lastN n = M $ \xs -> [swap $ splitAt (length xs - n) xs]

-- | Matches the lower end of a partial match.
firstN :: Int -> Matcher a
firstN n = M $ \xs -> [splitAt n xs]

-- | Discards input to the rest as long as the predicate does not fail.
dropWhile :: (a -> Bool) -> Matcher a 
dropWhile p = M $ \xs -> [swap $ span p xs]

-- | Matches input as long as the predicate does not fail.
takeWhile :: (a -> Bool) -> Matcher a
takeWhile p = M $ \xs -> [span p xs]

-- | Groups the list into successive segments and produces a match for each.
consecutiveBy :: (Eq b, Enum b) => (a -> b) -> Matcher a
consecutiveBy f = M $ \xs -> case groupSucc f xs of
    []     -> []
    g : gs -> go [] g gs
  where
    go :: [[a]] -> [a] -> [[a]] -> [([a], [a])]
    go l m []         = [(m, concat $ reverse l)]
    go l m r@(x : xs) = (m, concat $ reverse l ++ r) : go (m : l) x xs

-- | Splits the match into all possible values occuring in the input after
-- applying the translation function on it.
splitBy :: Ord b => (a -> b) -> Matcher a
splitBy f = M $ \xs -> map (`extract` xs) $ ordNub $ map f xs
  where
    extract y = L.partition ((== y) . f)
    ordNub = map head . L.group . L.sort

-- | Matches all elements which satisfy the predicate.
filter :: (a -> Bool) -> Matcher a
filter p = M $ (: []) . L.partition p
