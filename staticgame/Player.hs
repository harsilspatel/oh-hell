module Player (
    playCard,
    makeBid
)
where
{-
How this file is organised:
    1. All the functions are organised in a way that the simpler functions, such as getRank and getSuit, appear before the composite
       functions which call the simpler functions, such as leadSuit. The advantage of adapting this convention is as reader will
       read from top to bottom, it will ensure that he/she is familiar with the simpler functions before the composite function calls them.
    2. The functions called from other (not-standard) modules are written using the dot-notation, for example,
       the makeBid function calls hookRule from ohHell hence it is written ohHell.hookRule
       Doing so will help reader know which functions are defined in Player and which are not.

Strategy:
    Overview:
    The bot plays its cards according to whether or not it has won as many tricks as it bid.

    Bidding:
    Roughly, the number of trump cards and the aces in hand gauge the number of tricks the bot would win.
    The higher ranking trump card will, statically, have more chances of being the highest 
    ranking card in the trick, however, one can argue that the lower ranking trump cards have 
    low chances of winning. So then the question arises, "Why does the bot takes the number of
    trump card (and aces) as the reference for bidding?" Well the answer is, it might not win using the 
    low ranking trump cards but then chances are that there will be high ranking cards of non-trump
    suits. So, in a probabilistic sense, the chances of winning by high-ranking non-trumps nullify
    the chances of losing by low-ranking trumps.

    Card play:
    The card-choosing strategy of bot is fairly simple, if the bot has won as many as it bid then
    it will try to play the minimum card to let others win the tricks in order to have its wins = bids.
    If it is not the case the winnings match the bid, then the bot will try to play higher cards in order to
        i)  Try to reach the bid amount. (if win was < bid) OR
        ii) In the case where the number of winning tricks has already exceeded the bid amount then it will
            try and sabotage other bots' chances of winning the tricks to prevent them from reaching their
            bid amounts.
            The strategy to obstruct other players' winning is not effective when they are bidding low amounts,
            in fact, in some scenarios it can be beneficial to them since our bot may help them by making sure
            that they don't win more tricks. However, as most bots would want to win more - they would bid more,
            therefore, in most cases, the gamble of implementing this strategy pays off.


    Conclusion: This many not be the best strategy, however, after being among the top 5 on the ladder,
    for more than 3 days, I am confident that this is an effective strategy.
-}



import OhTypes
import OhHell

-- | getRank of the Card
getRank :: Card -> Rank
getRank (Card _ rank) = rank 

-- | getSuit of the Card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- | get the lead suit from [Card].
-- [Card] is just PlayerIds removed from Trick
leadSuit :: [Card] -> Suit
leadSuit cs = getSuit $ last cs

-- | From the given [Card] filter those which match the Suit
cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cs s = filter ((s==) . getSuit) cs

-- | Drop the PlayerIds from trick and return [Card]
cardsFromTrick :: Trick -> [Card]
cardsFromTrick t = map fst t

-- | Counting myWins using the winner from OhHell
myWins :: PlayerId -> Suit -> [Trick] -> Int
myWins pID tSuit ts = length(filter (pID==) (map (OhHell.winner tSuit) ts))

-- | Getting myBid from the bids
myBid :: PlayerId -> [(PlayerId,Int)] -> Int
myBid pID bs = snd $ head $ (filter((pID==).fst) bs)

-- | quicksort the [Card] by their rank
sortByRank :: [Card] -> [Card]
sortByRank [] = []
sortByRank (c:cs) = (sortByRank lower) ++ [c] ++ (sortByRank higher)
    where
        lower = filter((< getRank c) . getRank) cs
        higher = filter((>= getRank c). getRank) cs

-- | the function to play high ranking card
tryToWin :: [Card] -> Suit -> [Card] -> Card
tryToWin cs tSuit playedCards              
    | null playedCards = last $ sortByRank cs  -- if my bot is first to play then choose any card with highest rank
    | not $ null ledSuitCards = maximum ledSuitCards -- else get highest ranking card of lead suit
    | not $ null trumpSuitCards = minimum trumpSuitCards -- or get minimum trumpSuitCard (here I can have "maximum trumpSuitCards" but chances are that
                                                         -- someone else can have a higher card than my highest trump, but then it is also possible that
                                                         -- no one else plays trump so I can just by playing minimum trump get the whole trick)
    | otherwise = head $ sortByRank otherCards
    where
        ledSuit = leadSuit playedCards
        ledSuitCards = cardsOfSuit cs ledSuit
        trumpSuitCards = cardsOfSuit cs tSuit
        otherCards = filter (\c -> getSuit c /= tSuit && getSuit c /= ledSuit) cs

-- | the function to play low ranking card 
dontWin :: [Card] -> Suit -> [Card] -> Card
dontWin cs tSuit playedCards             
    | null playedCards = head $ sortByRank cs  -- if my bot is first to play then choose any card with lowest rank
    | not $ null ledSuitCards = minimum ledSuitCards  -- else get lowest ranking card of lead suit
    | not $ null otherCards = last $ sortByRank otherCards -- else high ranking card of any other suit, so that we do not win any other trick using that card.
    | otherwise = minimum trumpSuitCards -- if nothing then lowest trump
    where
        ledSuit = leadSuit playedCards
        ledSuitCards = cardsOfSuit cs ledSuit
        trumpSuitCards = cardsOfSuit cs tSuit
        otherCards = filter (\c -> getSuit c /= tSuit && getSuit c /= ledSuit) cs

-- | playCard decides whether to the bot should tryToWin or tells the bot - `dontWin`
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

-- | the bid function that tests if theBid follows HookRule
-- if it doesn't then it tries to decrement the value
-- if it cannot, because of the zero as lower bound, then
-- it will increment theBid.
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