import Control.Applicative hiding ((<|>))
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.String
import Data.Maybe

data Date = Date Int Int Int deriving (Show)
data Time = Time Int Int Double deriving (Show)
data File = File Name LineNb Tid deriving (Show)
data Segment = Segment Date Time Host Code Level File Message deriving (Show)

type Host = String
type Code = String
type Level = String
type Name = String
type LineNb = Int
type Tid = Int
type Message = String

dateSep = char '/'

date :: Parser Date
date = do
    y <- many1 digit
    dateSep
    m <- many1 digit
    dateSep
    d <- many1 digit
    return $ Date  (read y) (read m) (read d)

timeSep = char ':'

time :: Parser Time
time = do
    h <- many1 digit
    timeSep
    m <- many1 digit
    timeSep
    s <- many1 (char '.' <|> digit)
    return $ Time (read h) (read m) (read s)

host :: Parser Host
host = do
    h <- many1 alphaNum
    return h

code :: Parser Code
code = do
    c <- many1 upper
    return c

level :: Parser Level
level = do
    h <- many1 upper
    return h

fileName :: Parser Name
fileName = do
    n <- many1 (char '.'  <|> alphaNum)
    return n

lineNumber :: Parser LineNb
lineNumber = do
    l <- many1 digit
    return $ read l

tid :: Parser Tid
tid = do
    string "TID#"
    t <- many1 digit
    return $ read t

file :: Parser File
file = do
    char '<'
    n <- fileName
    char '#'
    l <- lineNumber
    spaces
    t <- tid
    char '>'
    return $ File n l t

message :: Parser Message
message = do
    m <- many1 anyChar
    return m

segment :: Parser Segment
segment = do
    d <- date
    spaces
    t <- time
    spaces
    h <- host
    spaces
    c <- code
    spaces
    l <- level
    spaces
    f <- file
    spaces
    m <- message
    return $ Segment d t h c l f m


parseSegment :: String -> Maybe Segment
parseSegment input = case result of
    Left _        -> Nothing
    Right segment -> Just segment
    where result = parse segment  "Segment Parser" input

parseSegments :: [String] -> [Segment]
parseSegments = mapMaybe parseSegment

main :: IO ()
main = do
    (file:_) <- getArgs
    content <- readFile file
    let segments = parseSegments $ lines content
    mapM_ print segments

