module Player (
    playCard,
    makeBid
)
where

import OhTypes
import OhHell

getRank :: Card -> Rank
getRank (Card _ rank) = rank 

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

leadSuit :: Trick -> Suit
leadSuit cs = getSuit $ fst $ last cs

cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cs s = filter ((s==) . getSuit) cs

-- cardsFromTrick :: Trick -> [Card]
-- cardsFromTrick t = map (fst) t

compareCard :: Card -> Card -> Suit -> Ordering
compareCard c1 c2 tSuit
    | getSuit c1 == getSuit c2 = getRank c1 `compare` getRank c2
    | getSuit c1 == tSuit = GT
    | getSuit c2 == tSuit = LT
    | otherwise = EQ

higherCard :: Card -> Card -> Suit -> Card
higherCard c1 c2 tSuit
    | compareCard c1 c2 tSuit == LT = c2    -- if c2 is higher than return it
    | otherwise = c1                    -- if c1 is higher or equal to c2 return c2

highestCard :: [Card] -> Suit -> Card
highestCard cs tSuit = foldr1(\c1 c2 -> higherCard c1 c2 tSuit) cs  -- using foldr1 as we know cards in hand are never going to be 0 (and when they will be playCard will not be called!)

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