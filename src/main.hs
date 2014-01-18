

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
matchTask (Options ns ss ds) (Task n d _ s _) =
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

-- TODO better way to implement ordering? "<>"s needs escaping in most shells.
-- TODO better way to implement ordering for datetimes? intervals?
-- TODO more detailed descriptions

-- opts:
--  --name[!]=expr : e.g. --name="agroup.*", --name!="agroup.*.mytask", etc.
--  --stressOPnum : eg. --stress=1, --stress>0, etc.
--  --datetimeOPdatetime : e.g. --datetime<'2014-01-01 00:00', etc.
-- TODO opts:
--  --prependfilenames : task "agroup.mytask" in file "f" ==> "f.agroup.mytask"
--  --with-complete : include completed tasks
--  --without-complete : don't include completed tasks (default?)
main = do
    args <- getArgs
    (opts, filenames) <- parseOptions args
    taskLists <- mapM getTasks filenames
    tasks <- return $ L.sort $ concat taskLists
    ftasks <- return $ filter (matchTask opts . fst) tasks
    print ftasks
    

