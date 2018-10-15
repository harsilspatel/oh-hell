module Player (
    playCard,
    makeBid
)
where

-- abbrviations:
-- b = bid
-- c = cards
-- p = players
-- s = suit
-- t = tricks



import OhTypes
import OhHell

getRank :: Card -> Rank
getRank (Card _ rank) = rank 

getSuit :: Card -> Suit
getSuit (Card suit _) = suit

leadSuit :: [Card] -> Suit
leadSuit cs = getSuit $ last cs

cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cs s = filter ((s==) . getSuit) cs

cardsFromTrick :: Trick -> [Card]
cardsFromTrick t = map fst t

compareCard :: Suit -> Card -> Card -> Ordering
compareCard tSuit c1 c2 
    | getSuit c1 == getSuit c2 = getRank c1 `compare` getRank c2
    | getSuit c1 == tSuit = GT
    | getSuit c2 == tSuit = LT
    | otherwise = EQ

higherCard :: Suit -> Card -> Card -> Card
higherCard tSuit c1 c2
    | compareCard tSuit c1 c2 == LT = c2    -- if c2 is higher than return it
    | otherwise = c1                        -- if c1 is higher or equal to c2 return c2

highestCard :: [Card] -> Suit -> Card
highestCard cs tSuit = foldr1(\c1 c2 -> higherCard tSuit c1 c2) cs  -- using foldr1 as we know cards in hand are never going to be 0 (and when they will be playCard will not be called!)

lowestCard :: [Card] -> Suit -> Card
lowestCard cs tSuit = foldr1(\c1 c2 -> if compareCard tSuit c1 c2 == GT then c2 else c1) cs

myWins :: PlayerId -> Suit -> [Trick] -> Int
myWins pID tSuit ts = length(filter (pID==) (map (OhHell.winner tSuit) ts))

-- to correct $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
myBid :: PlayerId -> [(PlayerId,Int)] -> Int
myBid pID bs = snd $ head $ (filter((pID==).fst) bs)

tryToWin :: [Card] -> Suit -> [Card] -> Card
tryToWin cs tSuit playedCards
    | not $ null ledSuitCards = highestCard ledSuitCards (leadSuit playedCards)
    | not $ null trumpSuitCards = highestCard trumpSuitCards tSuit
    | otherwise = lowestCard cs (head $ filter(\s -> tSuit /= s && (leadSuit playedCards) /= s) [Spade ..])
    where
        ledSuitCards = cardsOfSuit cs (leadSuit playedCards)
        trumpSuitCards = cardsOfSuit cs tSuit

dontWin :: [Card] -> Suit -> [Card] -> Card
dontWin cs tSuit playedCards
    | not $ null ledSuitCards = lowestCard ledSuitCards (leadSuit playedCards)
    | otherwise = highestCard cs (head $ filter(tSuit/=) [Spade ..])
    where
        ledSuitCards = cardsOfSuit cs (leadSuit playedCards)

-- playSomething :: [Card] -> Suit -> Card
-- playSomething cs ledSuit
--     | not $ null ledSuitCards = head ledSuitCards
--     | otherwise = head cs
--     where
--         ledSuitCards = cardsOfSuit cs ledSuit

playCard :: PlayFunc
playCard pID cs bs trump ts thisTrick
    --  = playSomething cs (getSuit trump)
    | myBidValue > myWinValue = tryToWin cs trumpSuit currentCards
    | otherwise = dontWin cs trumpSuit currentCards
    where
        trumpSuit = (getSuit trump)
        myBidValue = myBid pID bs 
        myWinValue = myWins pID trumpSuit ts
        currentCards = cardsFromTrick thisTrick

makeBid :: BidFunc
makeBid trump cs ps bids
    | followsHookRule = trumpCardsCount
    | trumpCardsCount-1 >= 0 = trumpCardsCount-1
    | otherwise = trumpCardsCount+1
    where
        trumpSuit = getSuit trump
        trumpCards = cardsOfSuit cs trumpSuit
        trumpCardsCount = length trumpCards
        followsHookRule = OhHell.hookRule (bids++[trumpCardsCount]) ps (length cs)