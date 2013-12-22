
module Tasker
( module Tasker -- TODO
) where

import Text.ParserCombinators.Parsec
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Function (on)
import Data.List (partition, isPrefixOf, nub, groupBy, sortBy, intercalate)


type Deadline = Maybe UTCTime

-- | The Task datatype.
-- Can be a single task or a group of tasks.
-- Note that the name is used as a key -
-- different tasks should not have the same name!
data Task = Task { name :: String,
                   deadline :: Deadline,
                   description :: String,
                   dependencies :: [String] }
          deriving Show

-- TODO test deps
-- TODO deps: Data.Map.Map?

instance Eq Task where
    (==) = (==) `on` name


-- | Compares two Tasks by their deadline.
--
-- Earlier deadlines are ordered before later deadlines.
-- 'Just's are ordered before 'Nothing'.
cmpDeadline :: Task -> Task -> Ordering
cmpDeadline (Task _ ad _ _) (Task _ bd _ _) = cmp ad bd
    where cmp Nothing Nothing = EQ
          cmp x Nothing = GT
          cmp Nothing y = LT
          cmp (Just x) (Just y) = compare x y


-- ------------------------------------------------------------------------- --
-- ==================================PARSER================================= --
-- ------------------------------------------------------------------------- --

data RawTask = Tt String Deadline String
             | Tg String [RawTask]
             | Td String [String]
             deriving (Show, Eq, Ord)

ttname (Tt n _ _) = n

-- | Parses p then s and returns the result from p.
followedBy p s = do {x <- p; s; return x}

-- | Parses a space, @' '@.
sp = char ' '
sps = many sp
sps1 = many1 sp

-- | Parser for space, @' '@, or newline, @'\n'@.
-- I prefer this cleaner approach to Parsec's 'space'
sl = oneOf " \n"
sls = many sl
sls1 = many1 sl

-- | Parses a task-name.
-- Allowed chars: @a-z@, @A-Z@, @0-9@, @-@, @_@.
nameParser :: Parser String
nameParser = many1 $ alphaNum <|> oneOf "-_"

task = do
    string "task"
    sps1
    nsdt <- taskNames
    d <- option "" descriptionParser
    return $ map (\(n,dt) -> Tt n dt d) nsdt

taskNames :: Parser [(String, Deadline)]
taskNames = sepBy1 np sp
    where np = do
            n <- nameParser; sps
            dt <- option Nothing deadlineParser; sps
            return (n,dt)
          sp = char ',' `followedBy` sls
descriptionParser = do
    string ":"
    option "" $ do
        sp
        many $ noneOf "\n"
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



group = do
    string "group"
    sps1
    n <- nameParser
    sps
    ts <- option [] $ between (char '{') (char '}') items
    return $ [Tg n ts]



dep = do
    string "dep"
    sps1
    ns <- depNames
    sps
    char ':'
    sps
    deps <- depNames
    return $ map (`Td` deps) ns
depNames = sepBy1 np sep
    where np = nameRef `followedBy` sps
          sep = char ',' `followedBy` sps
nameRef = sepBy1 np sep >>= return . intercalate "."
    where np = (oneOf "!*" >>= \x -> return [x]) <|> nameParser
          sep = char '.' `followedBy` sps



item :: Parser [RawTask]
item = task <|> group <|> dep -- n.b. ok since each starts with unique token.

items = do
    sls
    option [] $ do
        i <- item
        sps
        r <- option [] $ do
            newline
            items
        return (i++r)

-- | Parser for task-files.
-- See ../taskerGrammar.bnf
taskParser :: Parser [RawTask]
taskParser = do
    r <- items
    eof
    return $ r -- TODO refine here?

-- | Expands groups and applies deps.
refine :: [RawTask] -> [Task]
refine = fixDeps . expandTg

fixDeps :: [RawTask] -> [Task]
fixDeps rs = res
    where (tds,tt) = partition isTd rs
          isTd (Td _ _) = True
          isTd (Tt _ _ _) = False
          isTd (Tg _ _) = error "not supposed to happen"
          res = map toTask $ -- TODO
                groupBy ((==) `on` fst) $
                sortBy (compare `on` ttname . fst) $
                map (\x -> (x,[])) tt ++ concatMap (expandTd tt) tds
          toTask :: [(RawTask,[String])] -> Task
          toTask ts = Task n dt d $ nub $ concatMap snd ts
              where Tt n dt d = fst $ head ts


expandTd :: [RawTask] -> RawTask -> [(RawTask,[String])]
expandTd tt (Td n deps) = map (\x -> (x, r)) l
    where l = expandTd' tt n
          r = map ttname $ concatMap (expandTd' tt) deps

expandTd' :: [RawTask] -> String -> [RawTask]
expandTd' tt s = filter (matches . ttname) tt
    where matches y = and $ on (==) length a b : zipWith match a b
              where a = splitBy '.' s
                    b = splitBy '.' y
          match "*" _ = True
          match  x  y = x==y

splitBy _ [] = []
splitBy c s = splitBy' c s
splitBy' c s = x : if y==[] then [] else splitBy' c $ drop 1 y
    where (x,y) = span (/=c) s

-- | Expands groups.
-- This involves renaming Tt's after their parents
-- and expanding @'!'@ in Td-strings.
expandTg :: [RawTask] -> [RawTask]
expandTg = concatMap (expandTg' "")

expandTg' :: String -> RawTask -> [RawTask]
expandTg' p t = case t of
    Tt n dt d -> [Tt (normname p n) dt d]
    Tg n tt -> concatMap (expandTg' $ normname p n) tt
    Td n deps -> [Td (tdren n) (map tdren deps)]
    where normname "" x = x
          normname p x = p++'.':x
          tdren k = intercalate "." $ fix $ splitBy '.' $ normname p k
          fix [] = []
          fix (x:"!":s) = fix s
          fix ("!":s) = error "bad '!'."
          fix (x:s) = x : fix s


-- TODO REM
test s f = case parse taskParser  "" s of
    Right x -> mapM_ print $ f x
    Left err -> print err
