module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.

How this file is organised:
    1. All the functions are organised in a way that the simpler functions, such as getRank and getSuit, appear
       before the composite functions which call the simpler functions, such as leadSuit. The advantage of adapting this convention
       will ensure that reader is fimiliar with the simpler functions before the composite function calls them.
    2. The functions called from other (not-standard) modules are written using the dot-notation, for example,
       the makeBid function calls hookRule from ohHell hence it is written ohHell.hookRule
       Doing so will help reader know which functions are defined in Player and which are not.

Strategy:
Bidding

Playing

-}

import OhTypes
import OhHell

-- | Returns the Rank of the card
-- getRank :: Card -> Rank
-- getRank (Card _ rank) = rank

-- | Returns the Suit of the card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- | Returns the suit of leading card
-- copied from OhHell as it does not export this function
leadSuit :: Trick -> Suit
leadSuit cs = getSuit $ fst $ last cs

playSomething :: [Card] -> Suit -> Card
playSomething cs ledSuit =
    let
        cardsOfSuit suit = filter ((suit==) . getSuit) cs
        ledSuitCards = cardsOfSuit ledSuit
    in
        if not $ null ledSuitCards
        then head ledSuitCards
        else head cs

-- | Play a card for the current trick.
-- If you are the "lead" player, you must follow the suit of the card that was led.
playCard :: PlayFunc
playCard _ cs _ _ _ playedCards = if null playedCards then head cs else playSomething cs (leadSuit playedCards)



-- | Bid the number of cards you can win based on the trump card and your hand.
--   last player to bid must obey "hook rule":
--   sum of bids must not equal number of tricks available
makeBid :: BidFunc
makeBid trump cs players bids =
    let
        cardsOfSuit suit = filter ((suit==) . getSuit) cs
        trumpSuit = getSuit trump
        trumpCards = cardsOfSuit trumpSuit
        trumpCardsCount = length trumpCards
    in
        if OhHell.hookRule (bids++[trumpCardsCount]) players (length cs)
            then trumpCardsCount
            else (
                if trumpCardsCount-1 >= 0
                    then trumpCardsCount-1
                    else trumpCardsCount+1
            )