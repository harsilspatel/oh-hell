-- naive player

module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.
-}

import OhTypes
import OhHell

-- leadingCard :: [Card] -> Card
-- leadingCard cards = last cards

playSomething :: [Card] -> Suit -> Card
playSomething cs ledSuit =
    let
        cardSuit (Card suit _) = suit
        cardsOfSuit suit = filter ((suit==) . cardSuit) cs
        ledSuitCards = cardsOfSuit ledSuit
    in
        if not $ null ledSuitCards
        then head ledSuitCards
        else head cs

xleadSuit :: Trick -> Suit
xleadSuit cs = let (Card suit _, _) = last cs in suit

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard _ cs _ _ _ playedCards = if null playedCards then head cs else playSomething cs (xleadSuit playedCards)



-- | Bid the number of cards you can win based on the trump card and your hand.
--   last player to bid must obey "hook rule":
--   sum of bids must not equal number of tricks available
makeBid :: BidFunc
makeBid _ cs players bids =
    if hookRule (bids++[0]) players (length cs)
    then 0
    else 1