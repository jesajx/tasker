

import Tasker
import System.IO ( stdin, stdout
                 , openFile, hClose
                 , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import qualified Data.List as L
import Data.Time

data Options = Options
    { nameExprs :: [(String,Bool)]
    , stressExprs :: [(Int, Ordering)]
    , datetimeExpr :: [(datetime, Ordering)]  -- TODO intervals?
    }

-- TODO use othertype then Ordering. to implement >=, <= and <> ?

defaultOptions now = Options
    { nameExpr = []
    , stressExpr = []
    , datetimeExpr :: [(now, GT)]   -- EQ too?
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['n'] ["name"] (ReqArg \n -> (n, True))
        "Expression to whitelist full tasknames. May be used multiple times."
    , Option ['m'] ["notname"] (ReqArg \n -> (n, False))
        "Expression to blacklist full tasknames. May be used multiple times."
    , Option ['s'] ["stress"] (ReqArg parseOptStress)
        "filter stress."
    , Option ['d'] ["datetime"] (ReqArg parseOptDatetime)
        "filter datetime."
    ]

-- TODO how to implement operators? --stress=1 >1 <1 !=1 <>1 >=1 <=1
-- TODO better way to implement ordering? "<>"s needs escaping in most shells.
-- TODO better way to implement ordering for datetimes? intervals?
-- TODO more detailed descriptions

-- TODO opts
--  --name=expr : e.g. --name="agroup.*", --name="agroup.*.mytask", etc.
--  --notname=expr : e.g. --notname "agroup.*.mytask"
--  --stress op num : eg. stress=1, stress>0, etc.
--  --datetime op datetime : e.g. datetime<'2014-01-01 00:00', etc.
--
--  unnecessary?:
--   --with-complete : include completed tasks
--   --without-complete : don't include completed tasks (default?)
main = do
    args <- getArgs
    case length args of
        0 -> printTaskList "-"
        1 -> printTaskList (args !! 0)
        _ -> error "too many files!"

printTaskList filename = do
    cont <- if filename == "-" then getContents else readFile filename
    case parse taskParser filename cont of
        Right ts -> let ss = L.sort $ expandRaw ts
                    in mapM_ print $ sortTasks ss
        Left err -> print err

