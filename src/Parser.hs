module Parser where

import Prelude
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Traversable

import Instances
import OhHell
import System.IO
import OhTypes

-- | A parser that always fails with the given error.
failed :: ParseError -> Parser a
failed pe = P (\_ -> Error pe)

-- | Produces a parser that always fails with @UnexpectedChar@ using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser c = failed (UnexpectedChar c)

-- | A parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
character :: Parser Char
character = P (\i -> if length i == 0 then Error UnexpectedEof else Result (tail i) (head i))
    

-- | A parser that asserts that there is no remaining input.
eof :: Parser ()
eof = P (\i -> if length i > 0 then Error (UnexpectedString i) else Result "" ())

-- | A parser that tries the first parser for a successful value, then:
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P(\i -> if isErrorResult(parse p1 i) then parse p2 i else parse p1 i)
-- (|||) (P f1) (P f2) = P(\i -> if isErrorResult(f1 i) then f2 i else f1 i)


infixl 3 |||

-- | A parser that continues producing a list of values from the given
-- parser.
list :: Parser a -> Parser [a]
list k = list1 k ||| pure []

-- | A parser that produces at least one value from the given parser
-- then continues producing a list of values from the given parser (to
-- ultimately produce a non-empty list).

list1 :: Parser a -> Parser [a]
list1 p = 
    p >>= (\i -> (list p) >>= (\j -> pure (i : j)))

-- | A parser that produces a character but fails if:
--   * the input is empty; or
--   * the character does not satisfy the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = character >>= (\i -> if f i then pure i else unexpectedCharParser i)

-- | A parser that produces the given character but fails if:
--   * the input is empty; or
--   * the produced character is not equal to the given character.
is :: Char -> Parser Char
is c = satisfy (\i -> c == i)

-- | A parser that produces any character but fails if:
--   * the input is empty; or
--   * the produced character is equal to the given character.
isNot :: Char -> Parser Char
isNot c = satisfy (\i -> c /= i)

-- | A parser that produces a character between '0' and '9' but fails if
--   * the input is empty; or
--   * the produced character is not a digit.
digit :: Parser Char
digit = satisfy isDigit

digits :: Parser String
digits = list digit

-- | A parser that produces a space character but fails if
--   * the input is empty; or
--   * the produced character is not a space..
space :: Parser Char
space = satisfy isSpace

-- | A parser that will parse zero or more spaces.
spaces :: Parser String
spaces = list space

-- | A parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--   * the input is empty; or
--   * the first produced character is not a space.
spaces1 :: Parser String
spaces1 = list1 space

-- | A parser that produces a lower-case character but fails if:
--   * the input is empty; or
--   * the produced character is not lower-case.
lower :: Parser Char
lower = satisfy isLower

-- | A parser that produces an upper-case character but fails if:
--   * the input is empty; or
--   * the produced character is not upper-case.
upper :: Parser Char
upper = satisfy isUpper

-- | A parser that produces an alpha character but fails if:
--   * the input is empty; or
--   * the produced character is not alpha.
alpha :: Parser Char
alpha = satisfy isAlpha

alphas :: Parser String
alphas = list alpha

alphaNum :: Parser Char
alphaNum = alpha ||| digit

alphaNums :: Parser String
alphaNums = list alphaNum

-- | A parser that sequences the given list of parsers by producing all
-- their results but fails on the first failing parser of the list.
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = pure []
sequenceParser (p:ps) = p >>= (\i -> (sequenceParser ps) >>= (\is -> pure (i:is)))

-- | A parser that produces the given number of values off the given
-- parser.  This parser fails if the given parser fails in the attempt to
-- produce the given number of values.
thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequenceParser(replicate n p)

-- | A function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
tok :: Parser a -> Parser a
tok p = do
    v <- p
    spaces
    pure v

-- | A function that parses the given char followed by 0 or more spaces.
charTok :: Char -> Parser Char
charTok c = tok (is c)

-- | A parser that parses a comma ',' followed by 0 or more spaces.
commaTok :: Parser Char
commaTok = charTok ','

-- | A function that parses the given string (fails otherwise).
string :: String -> Parser String
string s = traverse is s

-- | A function that parses the given string, followed by 0 or more
-- spaces.
stringTok :: String -> Parser String
stringTok s = tok $ string(s)

-- | A function that produces a non-empty list of values coming off the
-- given parser (which must succeed at least once), separated by the second
-- given parser.
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 p2 = do
    i <- p1
    j <- list(p2 *> p1)
    pure (i:j)

-- | A function that produces a list of values coming off the given
-- parser, separated by the second given parser.
sepby :: Parser a -> Parser s -> Parser [a]
sepby p1 p2 = sepby1 p1 p2 ||| pure []

-- | A function that parses one of the characters in the given string.
oneof :: String -> Parser Char
oneof s = satisfy(\i -> elem i s)

-- | A function that parses any character, but fails if it is in the
-- given string.
noneof :: String -> Parser Char
noneof s = satisfy(\i -> notElem i s)

between :: Parser s -> Parser e -> Parser a -> Parser a
between start end p = do
    start
    v <- p
    end 
    pure v

semiColonTok :: Parser Char
semiColonTok = charTok ';'

toSuit :: Char -> Suit
toSuit c = case c of
    'S' -> Spade
    'C' -> Club
    'D' -> Diamond
    'H' -> Heart

suit :: Parser Suit
suit = P check
    where
        check i
            | head i == 'S' = Result (tail i) (Spade)
            | head i == 'C' = Result (tail i) (Club)
            | head i == 'D' = Result (tail i) (Diamond)
            | head i == 'H' = Result (tail i) (Heart)
            | otherwise = Error (UnexpectedChar (head i))

-- ASCII 2 = 50
-- Rank Two = 0
toRank :: String -> Rank
toRank c
    | length c == 2 = Ten
    | c == "J" = Jack
    | c == "Q" = Queen
    | c == "K" = King
    | c == "A" = Ace
    | otherwise = toEnum(r-50) :: Rank
    where
        r = fromEnum(head c)

rank :: Parser Rank
rank = P (\i -> if elem (toRank i) [Two ..] then Result "" (toRank i) else Error UnexpectedEof)

card :: Parser Card
-- card = (Card <$> suit) <*> rank
card = do
    s <- alpha
    r <- list1 alphaNum
    pure (Card (toSuit s) (toRank r))

toTrick :: [Card] -> Trick
toTrick cs = zip cs (map show [0..])

trick :: Parser Trick
trick = toTrick <$> sepby card commaTok

trickz :: Parser [Trick]
trickz = sepby trick semiColonTok

-- time,pos,bid,score,first,trump,tricks
line :: Parser (Int, String, Int, Int, String, Suit, [Trick])
line = do 
    time <- digits
    _ <- commaTok
    pos <- digits
    _ <- commaTok
    bid <- digits
    _ <- commaTok
    score <- digits
    _ <- commaTok
    first <- digits
    _ <- commaTok
    trump <- suit
    _ <- commaTok
    ts <- (between (is '"') (is '"') trickz)
    _ <- charTok '\r'
    pure (read time :: Int, pos, read bid :: Int, read score :: Int, first, trump, ts)

getResult :: ParseResult a -> a
getResult (Result _ result) = result

-- | An implementation of find
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:find
find :: (a -> Bool) -> [a] -> Maybe a
find f l
    | not $ null filtered = Just $ head $ filtered
    | otherwise = Nothing
    where
        filtered = filter f l


-- | An implementation of findIndex 
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:findIndex
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f l
    | not $ null filtered = Just $ snd $ head filtered
    | otherwise = Nothing
    where
        filtered = (filter (f.fst) (zip l [0..]))

adjustTrick :: PlayerId -> Trick -> Trick
adjustTrick pID t = drop index t ++ take index t
        where
            index = unwrapIndex $ findIndex ((pID==).snd) t
            unwrapIndex (Just x) = x+1 -- index needs to be adjusted as (take n l) will return 0..n-1 elements
            
adjustTricks :: PlayerId -> Suit -> [Trick] -> [Trick]
adjustTricks _ _ [] = []
adjustTricks pID tSuit (t:ts) = [adjustedTrick] ++ (adjustTricks winnerID tSuit ts)
    where
        winnerID = winner tSuit adjustedTrick
        adjustedTrick = adjustTrick pID t

winningsVsBids :: (Int, String, Int, Int, String, Suit, [Trick]) -> (Int, Int)
winningsVsBids (_, pos, bid, score, first, tSuit, ts)
    | score /= 0 = (bid, bid) -- if score is not zero, meaning my player won the game
    | otherwise = (winnings, bid)
    where
        winnings = unwrapItem $ find ((pos==).fst) everyonesWinnings
        everyonesWinnings = tallyTricks tSuit adjustedTrick
        adjustedTrick = adjustTricks pos tSuit ts
        unwrapItem Nothing = 0 -- if the player doesn't win anything tallyTricks doesn't have that players tuple with 0 wins, so we create a dummy tuple with zero wins.
        unwrapItem (Just x) = snd x

-- FileIO functions
getFile :: FilePath -> IO (FilePath, [String])
getFile fp = (\content -> pure (fp, drop 1 (lines content))) =<< (readFile fp) -- dropping the header

-- parseFile :: (FilePath, String) -> IO (FilePath, [String])
-- parseFile (fp, s) = pure (fp, drop 1 (lines s)) 

calcStats :: (FilePath, [String]) -> IO (FilePath, Int, Int, Int)
calcStats (fp, ls) = pure(fp, lessThan, wins, greaterThan)
    where
        parsedLines = (parse line) <$> ls
        lineResults = getResult <$> parsedLines
        lineWinVsBid = winningsVsBids <$> lineResults
        wins = length $ filter (\x -> fst x == snd x) lineWinVsBid
        lessThan = length $ filter (\x -> fst x < snd x) lineWinVsBid
        greaterThan = length $ filter (\x -> fst x > snd x) lineWinVsBid

printStats :: (FilePath, Int, Int, Int) -> IO ()
printStats (fp, lessThan, wins, greaterThan) = do
    putStrLn ("Stats of file: " ++ fp)
    putStrLn ("# of winnings: " ++ show wins)
    putStrLn ("# of times bot exceeded the bid: " ++ show greaterThan)
    putStrLn ("# of times bot could not reach the bid: " ++ show lessThan)


parserMain :: IO ()
parserMain = do
    putStrLn ("Please enter the filepath: ")
    fp <- getLine
    content <- getFile fp
    stats <- calcStats content
    printStats stats