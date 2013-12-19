
module Tasker
( Task(..)
, taskParser
, normalize
, cmpDeadline
) where

import Text.ParserCombinators.Parsec
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Function (on)



-- | The Task datatype.
-- Can be a single task or a group of tasks.
-- Note that the name is used as a key -
-- different tasks should not have the same name!
data Task = Task { name :: String,
                   deadline :: Maybe UTCTime,
                   description :: String }
          | TaskGroup { name :: String, tasks :: [Task] }
          deriving Show

instance Eq Task where
    (==) = on (==) name

-- | True iff the task is not a TaskGroup. Otherwise false.
isTask (Task _ _ _) = True
isTask _ = False

-- | True iff the task is a TaskGroup. Otherwise false.
isTaskGroup (TaskGroup _ _) = True
isTaskGroup _ = False

-- | Reorganizes a list of task, renaming Tasks and removing TaskGroups.
--
-- The name of each task is changed to reflect its parent group, like:
-- > newname = groupName++"."++taskName
-- This is done recursivly for taskGroups, replacing each taskgroup
-- with its children.
--
-- Example:
-- @
-- normalize
--   [Task "n" _ _,
--     TaskGroup "a" [Task "b", TaskGroup "c" [Task "d" _ _], TaskGroup "x" []]]
-- == 
-- [Task "n" _ _, Task "a.b", Task "a.c.d" _ _]
-- @
normalize :: [Task] -> [Task]
normalize = concatMap (normalize' "")

normalize' :: String -> Task -> [Task]
normalize' p t = case t of
    Task k dt d    -> [Task (normname p k) dt d]
    TaskGroup k tt -> concatMap (normalize' $ normname p k) tt
    where normname "" x = x
          normname p x = p++'.':x


-- | Compares two Tasks by their deadline.
--
-- Earlier deadlines are ordered before later deadlines.
-- 'Nothing' is orderered after everything else.
-- This is function for TaskGroups.
cmpDeadline :: Task -> Task -> Ordering
cmpDeadline (Task _ ad _) (Task _ bd _) = cmp ad bd
    where cmp Nothing Nothing = EQ
          cmp x Nothing = GT
          cmp Nothing y = LT
          cmp (Just x) (Just y) = compare x y


-- ------------------------------------------------------------------------- --
-- =================================PARSING================================= --
-- ------------------------------------------------------------------------- --

-- | Parses p then s and returns the result from p.
followedBy p s = do {x <- p; s; return x}

sp = char ' '
sps = many sp
sps1 = many1 sp

-- | Parses a task-name.
-- Allowed chars: a-z, A-Z, 0-9, \'-\', \'_\', \'.\'.
-- Note that \'.\' is allowed! See normalize.
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


-- | Parses datetimestring in format @[ 09:30  2013-12-27 ]@.
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

-- | Parser for task-files.
-- See ../taskerGrammar.bnf
taskParser :: Parser [Task]
taskParser = do
    r <- items
    eof
    return r


-- TODO REM
test s = case parse taskParser  "" s of
    Right x -> mapM_ print x
    Left err -> print err
