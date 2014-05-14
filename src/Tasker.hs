
module Tasker
( module Tasker -- TODO
) where

import Text.ParserCombinators.Parsec
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Function (on)
import qualified Data.List as L
import Data.Monoid
import qualified Data.Map as Map
-- TODO use Data.graph for depsorting?


-- | The Task datatype.
--
-- Note that the name is used as a key, i.e.
-- different tasks should not have the same name!
data Task = Task { name :: String
                 , startTime :: Maybe UTCTime
                 , stress :: Int
                 , description :: String
                 }

instance Show Task where
    show (Task n st s d) = n ++ replicate s '!' ++ dateStr ++ descrStr
        where dateStr = maybe "" (\x -> "[" ++ show x ++ "]") st
              descrStr = if d=="" then "" else ": " ++ d

-- | Tasks are equal if they have the same name.
instance Eq Task where
    (==) = (==) `on` name  -- TODO conform with Ord?

-- | Task ordering: 'startTime', 'stress', 'name', 'endTime' and 'description'.
instance Ord Task where
    compare a b = mconcat $
                  map (\f -> f a b)
                  [on cmpTime startTime,      -- Nothing last.
                   on (flip compare) stress,  -- Greater first.
                   on compare name,
                   on compare description]

-- TODO toTaskFile?

-- | Compares 'Maybe UTCTime' such that 'Nothing' sorts last.
cmpTime :: Maybe UTCTime -> Maybe UTCTime -> Ordering
cmpTime (Just x) (Just y) = compare x y
cmpTime x y = flip compare x y -- so Nothing sorts last

-- | Sorts tasks with dependencies.
-- TODO broken?
sortTasks :: [(Task, [String])] -> [(Task, [String])]
sortTasks ts = L.sortBy (cmp `on` fst) ts
    where cmp a b = depcmp a b `mappend` compare a b
          depcmp a b 
            | dependsOn a b = GT
            | dependsOn b a = LT
            | otherwise     = EQ
          ad = allDeps ts
          dependsOn x y = name y `elem` dd
              where Just dd = lookup x ad


allDeps :: [(Task,[String])] -> [(Task,[String])]
allDeps m = map (\(x,_) -> (x, L.nub $ allDeps' [] m x)) m

allDeps' :: [Task] -> [(Task,[String])] -> Task -> [String]
allDeps' p m x
    | x `elem` p = error $ "dep cycle: '" ++ show x ++ "'."
    | otherwise  = deps ++ concatMap (allDeps' (x:p) m . getByName) deps
    where Just deps = lookup x m
          getByName s =
            let Just k = lookup s $ map (\(y,_) ->  (name y, y)) m
            in k

-- | Calculates the \"urgency\" of a Task.
-- 
-- This is done by weighing the difference in time from 'd'
-- to the 'startTime' of the task by m:
-- @ m*((startTime t) - d) @
-- (and necessary type conversions).
urgency m d t = floor $ toRational $ maybe 0 weigh $ startTime t
    where weigh s = m*(diffUTCTime s d)

-- ------------------------------------------------------------------------- --
-- ==================================PARSER================================= --
-- ------------------------------------------------------------------------- --

data RawTask = Tt Task
             | Tg String [RawTask]
             | Td String [String]
             deriving (Show, Eq, Ord)

ttname (Tt (Task n _ _ _)) = n

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
    return $ map (\(n,ds,s) -> Tt $ Task n ds s d) nsdt

taskNames = sepBy1 np sp
    where np = do
            n <- nameParser
            s <- option 0 stressParser
            ds <- option Nothing (try taskTime)
            sps
            return (n,ds,s)
          sp = char ',' `followedBy` sls

stressParser = return . length =<< (many1 $ char '!')
    
descriptionParser = do
    string ":"
    option "" $ do
        sp
        many $ noneOf "\n"
taskTime = do
    char '['
    sps
    tp <- option Nothing datetime
    sps
    char ']'
    return tp
datetime = do
    d <- date
    t <- option "00:00" $ try $ sps1 >> time
    let fmt = "%Y-%m-%d %H:%M"
    return $ parseTime defaultTimeLocale fmt $ d ++ " " ++ t
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
nameRef = sepBy1 np sep >>= return . L.intercalate "."
    where np = (oneOf "!*%" >>= \x -> return [x]) <|> nameParser
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
    return $ r

-- | Expands groups and sorts into tasks and deps.
expandRaw :: [RawTask] -> [(Task,[String])]
expandRaw = expandDeps . expandGroups



-- | Expands Tt and Td into task-deps pairs.
expandDeps :: [RawTask] -> [(Task,[String])]
expandDeps rs = map toTask $
                L.groupBy ((==) `on` fst) $
                L.sortBy (compare `on` fst) $
                map (\x -> (x,[])) ns
                    ++ concatMap (expandTd ns) dd
    where (dd, tt) = L.partition isTd rs
          isTd (Td _ _) = True
          isTd (Tt _)   = False
          ts = map (\(Tt t) -> t) tt
          ns = map name ts
          toTask xs = (t, deps)
              where deps = L.nub $ concatMap snd xs
                    t = getTask $ fst $ head xs
                    getTask n = case L.find ((==n) . name) ts of
                            Just k  -> k
                            Nothing -> error $ "Can not find '" ++ n ++ "'."

-- | Expands wildcards in Td:s.
expandTd :: [String] -> RawTask -> [(String, [String])]
expandTd ns (Td dx dy) = map (\x -> (x, deps)) ts
    where ts = expandNameRef ns dx
          deps = concatMap (expandNameRef ns) dy

-- | Expands the given String (task nameref) into task names.
expandNameRef :: [String] -> String -> [String]
expandNameRef ns n = filter (on match (splitBy '.') n) ns
    where match [] []           = True
          match ("*":xs) ks     = s xs ks
          match ("%":xs) (k:ks) = match xs ks
          match (x:xs) (k:ks)   = x==k && match xs ks
          match _ _             = False
          s [] []     = True
          s _ []      = False
          s xs (k:ks) = match xs (k:ks) || s xs ks


splitBy _ [] = []
splitBy c s = splitBy' c s
splitBy' c s = x : if y==[] then [] else splitBy' c $ drop 1 y
    where (x,y) = span (/=c) s

-- | Expands groups.
-- This involves renaming Tt's after their parents
-- and expanding @'!'@ in Td-strings.
expandGroups :: [RawTask] -> [RawTask]
expandGroups = concatMap (expandTg "")

expandTg :: String -> RawTask -> [RawTask]
expandTg p t = case t of
    Tt (Task n sd r d) -> [Tt $ Task (normname p n) sd r d]
    Tg n tt   -> concatMap (expandTg $ normname p n) tt
    Td n deps -> [Td (tdren n) (map tdren deps)]
    where normname "" x = x
          normname p x = p++'.':x
          tdren k = L.intercalate "." $ fix $ splitBy '.' $ normname p k
          fix [] = []
          fix (x:"!":s) = fix s
          fix ("!":s) = error "bad '!'."
          fix (x:s) = x : fix s


-- TODO REM
test s f = case parse taskParser  "" s of
    Right x -> mapM_ print $ f x
    Left err -> print err
