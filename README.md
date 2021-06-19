# OhHell

Implementation of bot for [ohHell](https://en.wikipedia.org/wiki/Oh_Hell), a trick-taking card game in which the objective is to win exactly the number of tricks bid 🃏

## Algorithm:

### Bidding 
- `bid = # of aces + trump cards`
- bid is tweaked to obey hook rule (a constraint to ensure total of bids != # of tricks in that deal)

### Playing card
```py
if numberOfHandsWon != bid:
    # if numberOfHandsWon < bid then try to win a hand to get closer to bid
    # if numberOfHandsWon > bid, you've already lost at this point, 
    #    so try and win hands to keep others from winning their bids <--- this logic kept my bot in top 4 (in a class of 100+) all the time
    play the card with highest probability of winning
else:
    # numberOfHandsWon == bid, maintain your position to win the round
    play the lowest winning probability card
```
