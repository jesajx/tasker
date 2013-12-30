

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



-- TODO ops

main = do
    args <- getArgs
    let arg1 = args !! 0
    cont <- readFile arg1
    now <- getCurrentTime
    case parse taskParser arg1 cont of
        Right ts -> do
            putStrLn "###"
            mapM_ print ts
            let ss = L.sort $ expandRaw ts
            putStrLn "###"
            mapM_ print ss
            putStrLn "###"
            mapM_ print $ sortTasks ss
            putStrLn "###"
            mapM_ print $ allDeps $ map (\(x,xs) -> (name x, xs)) ss
            putStrLn "###"
            mapM_ print $ map (\(x,_) -> (x, prune ss x)) ss
        Left err -> print err

