module Poker.Pretty where

import Text.PrettyPrint.ANSI.Leijen

import Poker.Card

instance Pretty Suit where
    pretty s = char $ case s of
        Spades   -> '♠'
        Hearts   -> '♥'
        Diamonds -> '♦'
        Clubs    -> '♣'

instance Pretty Rank where
    pretty r = text $ case r of
        Jack  -> "Jack"
        Queen -> "Queen"
        King  -> "King"
        Ace   -> "Ace"
        r     -> show $ 2 + fromEnum r

instance Pretty Card where
    pretty (Card r s) = char '<' <> pretty r <+> text "of" <+> pretty s <> text "'s>"

