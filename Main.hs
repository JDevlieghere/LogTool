import Control.Applicative hiding ((<|>))
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.String
import Data.Maybe

displayBlue   = "\x1b[0;34m"
displayYellow = "\x1b[0;33m"
displayRed    = "\x1b[0;31m"
displayReset  = "\x1b[0m"

data Date = Date { dateYear :: Int
                 , dateMonth :: Int
                 , dateDay :: Int
                 }

instance Show Date where
    show (Date y m d) = show y ++ "/" ++ show m ++ "/" ++ show d

data Time = Time { timeHour :: Int
                 , timeMinute :: Int
                 , timeSecond :: Double
                 }

instance Show Time where
    show (Time  h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

data File = File { fileName :: Name
                 , fileLine :: LineNb
                 , fileTid :: Tid
                 }

instance Show File where
    show (File n l t) = "<" ++ n ++ " #" ++ show l ++ " PID" ++ show t ++ ">"

data Segment = Segment { segmentDate :: Date
                       , segmentTime :: Time
                       , segmentHost :: Host
                       , segmentCode :: Code
                       , segmentLevel :: Level
                       , segmentFile :: File
                       , segmentMessage :: Message
                       }

instance Show Segment where
    show (Segment d t h c l f m) = show d ++
                                   " " ++
                                   show t ++
                                   " " ++
                                   h ++
                                   " " ++
                                   c ++
                                   " " ++
                                   l ++
                                   " " ++
                                   show f ++
                                   " " ++
                                   m

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

fName :: Parser Name
fName = do
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
    n <- fName
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

filterFile :: [Segment] -> String -> [Segment]
filterFile segments file = filter hasName segments
    where hasName (Segment _ _ _ _ _ (File f _ _) _) = f == file

colorize :: Segment -> Segment
colorize s@(Segment d t h c l f m) = case l of
    "INFO" -> Segment d t h c l f (displayBlue ++ m ++ displayReset)
    "WARN" -> Segment d t h c l f (displayYellow ++ m ++ displayReset)
    "ERROR" -> Segment d t h c l f (displayRed ++ m ++ displayReset)
    _ -> s

main :: IO ()
main = do
    (file:filter:_) <- getArgs
    content <- readFile file
    let segments = parseSegments $ lines content
    mapM_ print $ map colorize segments

