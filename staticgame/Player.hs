module Player (
    playCard,
    makeBid
)
where
{-
For brevity the functions called from other (not-standard) modules are written using the dot-notation, for example, in makeBid function uses hookRule from ohHell hence writing it as ohHell.hookRule

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

-- copied from OhHell as it does not export this function
leadSuit :: Trick -> Suit
leadSuit cs = let (Card suit _, _) = last cs in suit

getRank :: Card -> Rank
getRank (Card _ rank) = rank

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

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
        cardSuit (Card suit _) = suit
        cardsOfSuit suit = filter ((suit==) . cardSuit) cs
        trumpSuit = cardSuit trump
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
    -- if OhHell.hookRule (bids++[0]) players (length cs)
    -- then 0
    -- else 1