import Data.Time
import Control.Applicative hiding ((<|>))
import Data.Text.Encoding
import System.Environment
import System.IO
import Data.Maybe
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Error
import qualified Text.Parsec as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

displayBlue   = "\x1b[0;34m"
displayYellow = "\x1b[0;33m"
displayRed    = "\x1b[0;31m"
displayReset  = "\x1b[0m"

type Host = ByteString
type Code = ByteString
type Level = ByteString
type Name = ByteString
type LineNb = Int
type Tid = Int
type LogMessage = ByteString

data File = File Name LineNb Tid deriving (Show)
data Segment = Structured UTCTime Host Code Level File LogMessage | Free LogMessage deriving (Show)


dateSep = P.char '/'
timeSep = P.char ':'

day :: Parser Day
day = do
    y <- P.many1 P.digit
    dateSep
    m <- P.many1 P.digit
    dateSep
    d <- P.many1 P.digit
    return $ fromGregorian  (read y) (read m) (read d)


time :: Parser TimeOfDay
time = do
    h <- P.many1 P.digit
    timeSep
    m <- P.many1 P.digit
    timeSep
    s <- P.many1 (P.char '.' <|> P.digit)
    return $ TimeOfDay (read h) (read m) (read s)

date :: Parser UTCTime
date = do
    d <- day
    spaces
    t <- time
    return $ UTCTime d (timeOfDayToTime t)

host :: Parser Host
host = do
    h <- P.many1 P.alphaNum
    return $ B.pack h

code :: Parser Code
code = do
    c <- P.many1 P.upper
    return $ B.pack c

level :: Parser Level
level = do
    h <- P.many1 P.upper
    return $ B.pack h

fName :: Parser Name
fName = do
    n <- manyTill anyChar (char '#')
    return $ B.pack n

lineNumber :: Parser LineNb
lineNumber = do
    l <- P.many1 P.digit
    return $ read l

tid :: Parser Tid
tid = do
    P.string "TID#"
    t <- P.many1 P.digit
    return $ read t

file :: Parser File
file = do
    P.char '<'
    n <- fName
    l <- lineNumber
    spaces
    t <- tid
    P.char '>'
    return $ File n l t

message :: Parser LogMessage
message = do
    m <- P.many1 P.anyChar
    return $ B.pack m

structuredSegment :: Parser Segment
structuredSegment = do
    d <- date
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
    return $ Structured d h c l f m

freeSegment :: Parser Segment
freeSegment = do
    m <- message
    return $ Free m

segment :: Parser Segment
segment = do
    s <- try structuredSegment <|> freeSegment
    return s

line :: B.ByteString -> Maybe Segment
line input = case result of
    Left _        -> Nothing
    Right segment -> Just segment
    where result = parse segment  "Segment Parser" input

parseLines :: [B.ByteString] -> [Segment]
parseLines = mapMaybe line

main :: IO ()
main = do
    (file:_) <- getArgs
    content <- B.readFile file
    let segments = parseLines $ B.lines content
    mapM_ print segments
