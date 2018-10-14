module Player (
    playCard,
    makeBid
)
where

import OhTypes
import OhHell

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- cardsFromTrick :: Trick -> [Card]
-- cardsFromTrick t = map (fst) t

leadSuit :: Trick -> Suit
leadSuit cs = getSuit $ fst $ last cs

cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cs s = filter ((s==) . getSuit) cs

playSomething :: [Card] -> Suit -> Card
playSomething cs ledSuit
    | not $ null ledSuitCards = head ledSuitCards
    | otherwise = head cs
    where
        ledSuitCards = cardsOfSuit cs ledSuit

playCard :: PlayFunc
playCard _ cs _ _ _ playedCards 
    | null playedCards = head cs 
    | otherwise = playSomething cs (leadSuit playedCards)

makeBid :: BidFunc
makeBid trump cs players bids
            | followsHookRule = trumpCardsCount
            | trumpCardsCount-1 >= 0 = trumpCardsCount-1
            | otherwise = trumpCardsCount+1
    where
        trumpSuit = getSuit trump
        trumpCards = cardsOfSuit cs trumpSuit
        trumpCardsCount = length trumpCards
        followsHookRule = OhHell.hookRule (bids++[trumpCardsCount]) players (length cs)
        