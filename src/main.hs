

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

-- TODO opts?
--  --name=expr : e.g. --name="agroup.*", --name="*.mytask", etc.
--  --notname=expr : e.g. --notname "mytask"
--  --with-complete : include completed tasks
--  --without-complete : don't include completed tasks (default?)
--  --stress op num : eg. stress=1, stress>0, etc.
--  --datetime op datetime : e.g. datetime<'2014-01-01 00:00', etc.
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

