

import Tasker
import System.IO ( stdin, stdout
                 , openFile, hClose , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import System.Console.GetOpt
import Text.ParserCombinators.Parsec (parse)
import qualified Data.List as L
import Data.Time
import Data.Function (on)
import Data.Maybe (fromMaybe)


gt :: (Ord a) => a -> a -> Bool
gt = (>)
lt :: (Ord a) => a -> a -> Bool
lt = (<)
eq :: (Ord a) => a -> a -> Bool
eq = (==)
ne :: (Ord a) => a -> a -> Bool
ne = (not .) . eq
ge :: (Ord a) => a -> a -> Bool
ge = (not .) . lt
le :: (Ord a) => a -> a -> Bool
le = (not .) . gt

data Options = Options
    { nameExprs :: [(String -> String -> Bool,String)]
    , stressExprs :: [(Int -> Int -> Bool,Int)]
    , datetimeExprs :: [(UTCTime -> UTCTime -> Bool,UTCTime)]
    }


defaultOptions = Options
    { nameExprs = []
    , stressExprs = []
    , datetimeExprs = []
    }

-- parseOperator :: (Ord a) => String -> (a -> a -> Bool, String)
parseOperator ('!':'=':n) = (ne,n)
parseOperator ('<':'=':n) = (le,n)
parseOperator ('>':'=':n) = (ge,n)
parseOperator ('=':n) = (eq, n)
parseOperator ('<':n) = (lt, n)
parseOperator ('>':n) = (gt, n)
parseOperator _ = error "bad operator!"


--parseOptName :: String -> (String -> String -> Bool, String)
parseOptName ('=':n) = (eq, n)
parseOptName ('!':'=':n) = (ne,n)
parseOptName _ = error "bad operator!"

--parseOptStress :: String -> ((Int -> Int -> Bool), Int)
parseOptStress opt = (b,read n) 
    where (b,n) = parseOperator opt


-- TODO add relative datetimes? like --datetime>=now, --datetime<friday
--parseOptDatetime :: String -> (UTCTime -> UTCTime -> Bool, UTCTime)
parseOptDatetime opt = (b,d)
    where (b,n) = parseOperator opt
          fmt = "%Y-%m-%d %H:%M" -- TODO globalize fmt in Tasker.hs?
          Just d = parseTime defaultTimeLocale fmt $ n


addNameExpr n (Options ns ss ds) = Options (n:ns) ss ds
addStressExpr s (Options ns ss ds) = Options ns (s:ss) ds
addDatetimeExpr d (Options ns ss ds) = Options ns ss (d:ds)


options :: [OptDescr (Options -> Options)] -- TODO
options =
    [ Option ['n'] ["name"]
        (ReqArg (addNameExpr . parseOptName) "EXPR")
        "Expression to filter full tasknames. May be used multiple times."
    , Option ['s'] ["stress"]
        (ReqArg (addStressExpr . parseOptStress) "EXPR")
        "Expression to filter stress. May be used multiple times."
    , Option ['d'] ["datetime"]
        (ReqArg (addDatetimeExpr . parseOptDatetime) "EXPR")
        "Expression to filter datetime. May be used multiple times."
    ]

parseOptions args = do
    now <- getCurrentTime
    case getOpt Permute options args of
        (o, p, []) -> return (foldl (flip id) defaultOptions o, p)
        (_, _, errs) -> ioError $ uerr errs
    where header = "Usage: tsk [OPTIONS...] tskfiles..."
          uerr errs = userError $ concat errs ++ usageInfo header options


-- | Tests a Task against given Options.
matchTask :: Options -> Task -> Bool
matchTask (Options ns ss ds) (Task n d s _) =
    (and $ map (\(b,m) -> b n m) ns)
    && (and $ map (\(b,m) -> b s m) ss)
    && matchDatetime ds d

matchDatetime ds Nothing = True
matchDatetime ds (Just d) = and (map (\(b,m) -> b d m) ds)

getTasks :: String -> IO [(Task,[String])]
getTasks filename = do
    cont <- if filename == "-" then getContents else readFile filename
    case parse taskParser filename cont of
        Right ts -> return $ expandRaw ts
        Left err -> error $ show err

-- TODO
taskItemString :: (Task, [String]) -> String
taskItemString ((Task n st s d), deps) =
    n ++ (replicate s '!') ++ dateStr ++ descrStr
    where dateStr = maybe "" (\x -> "[" ++ showTime x ++ "]") st
            where showTime = formatTime defaultTimeLocale fmt
                  fmt = "%Y-%m-%d %H:%M" -- TODO globalize fmt in Tasker.hs?
          descrStr
            | d == "" && deps == []     = ""
            | d == ""                   = ": " ++ depStr
            | deps == []                = ": " ++ d
            | otherwise                 = ": " ++ d ++ " " ++ depStr
          depStr = "@deps[" ++ L.intercalate ", " deps ++ "]"


-- TODO better way to implement ordering? "<>"s needs escaping in most shells.
-- TODO better way to implement ordering for datetimes? intervals?
-- TODO do filtering through complete expressions? 's>1||datetime>2014-01-30'
-- TODO more detailed descriptions

-- opts:
--  --expr=expr : e.g. --expr="name=agroup.*|date=2014-05-06", --expr="name!=agroup.*.mytask", etc.
-- TODO opts:
--  --prependfilenames : task "agroup.mytask" in file "f" ==> "f.agroup.mytask"
--  --nocompleted : don't include completed tasks ?
--  --no-print-deps : don't print deps for tasks
--  --stressed=c : when sorting by time sort instead ( time - stress * c )
--    etc. for different attributes..., stress,name,datetime,description,.etc
--  --filter=expr : e.g. 'name!=group.mytask & (datetime>today | stress > 1)'
--  --limit=N : print maximum the top n rows(tasks). unnecessary? head instead?
main = do
    args <- getArgs
    if length args == 0 then
            error $ usageInfo "Usage: tsk [OPTIONS...] tskfiles..." options
        else do
            (opts, filenames) <- parseOptions args
            taskLists <- mapM getTasks filenames
            tasks <- return $ sortTasks $ concat taskLists
            ftasks <- return $ filter (matchTask opts . fst) tasks
            mapM_ (putStrLn . taskItemString) ftasks


