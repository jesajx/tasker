
module Tasker
( Task(..)
, taskParser
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

-- TODO instances

-- | True iff the task is not a TaskGroup. Otherwise false.
isTask (Task _ _ _) = True
isTask _ = False

-- | True iff the task is a TaskGroup. Otherwise false.
isTaskGroup (TaskGroup _ _) = True
isTaskGroup _ = False

-- | Takes a list of tasks and taskgroups and returns a list of tasks.
-- The name of each task is changed to reflect its parent group,
-- like: groupName++"."++taskName
-- This is done recursivly for TaskGroups and no taskgroup
-- remains in the result.
normalize :: [Task] -> [Task]
normalize = concatMap (normalize' "")

normalize' :: String -> Task -> [Task]
normalize' p t = case t of
    Task k dt d    -> [Task (normname p k) dt d]
    TaskGroup k tt -> concatMap (normalize' $ normname p k) tt
    where normname "" x = x
          normname p x = p++'.':x




-- ------------------------------------------------------------------------- --
-- =================================PARSING================================= --
-- ------------------------------------------------------------------------- --


-- | Parses p then s and returns the result from p.
followedBy p s = do {x <- p; s; return x}

-- Because I don't like tab-chars among others.
sp = char ' '
sps = many sp
sps1 = many1 sp

-- | Parses a task-name.
-- Allowed chars: a-z, A-Z, 0-9, "-", "_", ".".
-- Note that "." is allowed! See normalize.
nameParser :: Parser String
nameParser = many1 $ alphaNum <|> oneOf "-_."

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


-- | Parses datetimestring in format "[ 09:30  2013-12-27 ]".
deadlineParser = do
    char '['
    sps
    dt <- option Nothing $ do
        st <- option "00:00" $ time `followedBy` sps1
        sd <- date
        let fmt = "%H:%M %Y-%m-%d"
        return $ parseTime defaultTimeLocale fmt $ st ++ ' ':sd
    sps
    char ']'
    return dt
time = do
    sh <- count 2 digit
    char ':'
    sm <- count 2 digit
    return $ sh++':':sm
date = do
    y <- count 4 digit
    char '-'
    m <- count 2 digit
    char '-'
    d <- count 2 digit
    return $ y++'-':m++'-':d
    


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

taskParser :: Parser [Task]
taskParser = do
    r <- items
    eof
    return r


-- -------------------------------------------------------------------------- --
-- ===================================MAIN=================================== --
-- -------------------------------------------------------------------------- --

main = do
    args <- getArgs
    let arg1 = args !! 0
    cont <- readFile arg1
    case parse taskParser arg1 cont of
        Right ts -> do
            putStrLn "tasks:"
            mapM_ print ts
            putStrLn "#norms:"
            mapM_ (print) $ normalize ts
        Left err -> print err


-- TODO REM
test s = case parse taskParser  "" s of
    Right x -> mapM_ print x
    Left err -> print err
