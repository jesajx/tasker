
module Tasker
( Task(..)
--, parseTaskFile
) where

import Text.ParserCombinators.Parsec
import Data.Functor ((<$>))
import Data.List (intercalate)

import System.IO ( stdin, stdout
                 , openFile, hClose
                 , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)
import Control.Monad (when)
import Data.Time

data Task = Task { name :: String,
                   deadline :: Maybe UTCTime,
                   description :: String }
          | TaskGroup { name :: String, tasks :: [Task] }
          deriving Show

isAtomTask (Task _ _ _) = True
isAtomTask _ = False

isTaskGroup (TaskGroup _ _) = True
isTaskGroup _ = False

------


sp = char ' '
sps = many sp
sps1 = many1 sp

-- | Parses p then s and returns the result from p.
followedBy p s = do {x <- p; s; return x}

-- | Parses one of the strings from the given list.
-- Empty list means error.
oneOfList ss = foldr1 (<|>) ss

-- | Repeats a parse n times and returns the results as a list.
rep n p
    | n<0 = error "rep n for n<0"
    | otherwise = rep' n p
rep' 0 _ = return []
rep' n p = do
    x <- p
    y <- rep (n-1) p
    return (x:y)

nameParser :: Parser String
nameParser = many1 alphaNum

taskNames :: Parser [(String, Maybe UTCTime)]
taskNames = sepBy1 np sp
    where np = do
            n <- nameParser; sps
            dt <- option Nothing deadlineParser; sps
            return (n,dt)
          sp = char ',' `followedBy` spaces

descriptionParser :: Parser String
descriptionParser = do
    string ":"
    option "" $ do
        sp
        many $ noneOf "\n"


-- Parses datetimestring in format "[ 09:30 2013-12-27 ]".
deadlineParser = do
    char '['
    sps
    s <- many $ digit <|> sp <|> oneOf ":-"
    sps
    char ']'
    return $ parseTime defaultTimeLocale fmt $ unwords $ words s --pad
    where fmt = "%H:%M %Y-%m-%d"


task :: Parser [Task]
task = do
    string "task"
    sps1
    nsdt <- taskNames
    d <- option "" descriptionParser
    return $ map (\(n,dt) -> Task n dt d) nsdt

    
group :: Parser [Task]
group = do
    string "group"
    sps1
    n <- nameParser
    sps
    ts <- option [] $ between (char '{') (char '}') items
    return $ [TaskGroup n ts]

item :: Parser [Task]
item = task <|> group -- TODO dep

items :: Parser [Task]
items = do
    spaces
    option [] $ do
        i <- item
        sps
        r <- option [] $ do
            newline
            items
        return (i++r)

taskFileParser = do
    r <- items
    eof
    return r


------

main = do
    args <- getArgs
    let arg1 = args !! 0
    cont <- readFile arg1
    case parse taskFileParser arg1 cont of
        Right t -> do
            putStrLn "#tasks/groups:"
            mapM_ print t 
        Left err -> print err


-- TODO REM
test s = case parse taskFileParser  "" s of
    Right x -> mapM_ print x
    Left err -> print err
