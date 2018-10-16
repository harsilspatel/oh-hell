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

-- gt :: Suit -> Card -> Card -> Card
-- gt tSuit c1 c2
--     | c1Suit == c2Suit = max c1 c2
--     | c1Suit == tSuit = c1
--     | c2Suit == tSuit 
--     where
--         c1Suit = getSuit c1
--         c2Suit = getSuit c2

leadSuit :: [Card] -> Suit
leadSuit cs = getSuit $ last cs

cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cs s = filter ((s==) . getSuit) cs

cardsFromTrick :: Trick -> [Card]
cardsFromTrick t = map fst t

myWins :: PlayerId -> Suit -> [Trick] -> Int
myWins pID tSuit ts = length(filter (pID==) (map (OhHell.winner tSuit) ts))

-- to correct $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
myBid :: PlayerId -> [(PlayerId,Int)] -> Int
myBid pID bs = snd $ head $ (filter((pID==).fst) bs)

sortByRank :: [Card] -> [Card]
sortByRank [] = []
sortByRank (c:cs) = (sortByRank lower) ++ [c] ++ (sortByRank higher)
    where
        lower = filter((< getRank c) . getRank) cs
        higher = filter((>= getRank c). getRank) cs

tryToWin :: [Card] -> Suit -> [Card] -> Card
tryToWin cs tSuit playedCards               -- $$$$ improve this so that it checks if following players have other cards or not
    | null playedCards = last $ sortByRank cs         -- $$$$ define a function to get the max
    | not $ null ledSuitCards = maximum ledSuitCards
    | not $ null trumpSuitCards = maximum trumpSuitCards
    | otherwise = head $ sortByRank otherCards
    where
        ledSuit = leadSuit playedCards
        ledSuitCards = cardsOfSuit cs ledSuit
        trumpSuitCards = cardsOfSuit cs tSuit
        otherCards = filter (\c -> getSuit c /= tSuit && getSuit c /= ledSuit) cs

dontWin :: [Card] -> Suit -> [Card] -> Card
dontWin cs tSuit playedCards                -- return highest card < the minimum.
    | null playedCards = head $ sortByRank cs
    | not $ null ledSuitCards = minimum ledSuitCards
    | not $ null otherCards = last $ sortByRank otherCards
    | otherwise = minimum trumpSuitCards
    where
        ledSuit = leadSuit playedCards
        ledSuitCards = cardsOfSuit cs ledSuit
        trumpSuitCards = cardsOfSuit cs tSuit
        otherCards = filter (\c -> getSuit c /= tSuit && getSuit c /= ledSuit) cs

playCard :: PlayFunc
playCard pID cs bs trump ts thisTrick
    --  = playSomething cs (getSuit trump)
    | myBidValue == myWinValue = dontWin cs trumpSuit currentCards
    | otherwise = tryToWin cs trumpSuit currentCards
    where
        trumpSuit = getSuit trump
        myBidValue = myBid pID bs 
        myWinValue = myWins pID trumpSuit ts
        currentCards = cardsFromTrick thisTrick

makeBid :: BidFunc
makeBid trump cs ps bids
    | followsHookRule = theBid
    | theBid-1 >= 0 = theBid-1
    | otherwise = theBid+1
    where
        trumpSuit = getSuit trump
        trumpCards = cardsOfSuit cs trumpSuit
        aces = filter ((Ace==).getRank) cs
        nonTrumpAces = filter((trumpSuit/=).getSuit) aces
        theBid = length trumpCards + length nonTrumpAces
        followsHookRule = OhHell.hookRule (bids++[theBid]) ps (length cs)