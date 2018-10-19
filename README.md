# OhHell
## Files Included:
- Parser.hs (src)
- Instances.hs (src)
- playerNaive.hs (players)
- playerNovice.hs (players)

## Files modified:
- OhHell.hs: to export the tallyTricks function that is used by Parser.hs

## Bot overview:

### Bidding 
- The bid is chosen based on the number of aces and trump cards
- Before returning the chosen bid, the function checks whether or not theBid obeys the HookRule, if it does not then it will decrement/increment it.

### Playing card
- Depending on whether the tricks won so far matches the bid amount or not, the bot chooses to play higher or lower cards
- It does not simply follow a greedy implementation, i.e. it does not play just the highest or the lowest card
- A comprehensive description is described in the Player.hs

## Extra:

### Generalisation
The Parser and the Player have been wriiten to play with <i>n</i> number of players and <i>m</i> number of cards. Although the given files will have to be modified for the whole game to support generalisation, for instance: OhHell.maxHand is hard-coded to 12.

### Parser
- The parser is an extension to week 10 and week 11 tutorial exercises.
- Parser get "first" from logs and rearranges the first trick such that "first" player's hand is last. We then pass it to OhHell.winner, which assumes that leading player's card is last. OhHell.winner will return the playerId of winner of that trick, so now that playerId is our "first" for the second trick. Therefore, we do this to the all tricks and rearrange them. We then pass the [Trick] to tallyTricks which will output the [(PlayerId, wins)]. The parser then compares the players wins with its bids and outputs the results. (Please note that parsing a 20-line file takes a fair bit of time.)
- The provided implementation of parser parses a log file to indicate how many times did the bot match its bid (won), the number of times it did not reach the bid and the number of times it exceeded the bid.
- Based on the parser reading of my previous version of my bot (playerNovice.hs), the one that only used to bid by trump cards, I learned that the number of times it exceeded the bid was fairly high, so to improve the bot, I tweaked funcitons in the following way:
		
		- Bid: The bid also considers non-trump aces
		- Playing: Implemented sortByRank to get highest and lowest ranking cards (so I can begin with highest ranking card if I'm the lead player)
- Last, but the most interesting discovery is that the provided code that which on executing <i>stack exec staticgame</i>, gives various stats such as number of wins and under and over bidding values are wrongs, and my parser's results prove that!


#### Code by: Harsil S. Patel
#### Student ID: 28334825