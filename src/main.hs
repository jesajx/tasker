

import Tasker
import System.IO ( stdin, stdout
                 , openFile, hClose
                 , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Data.List (sortBy, partition)

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
            mapM_ (print) $ refine ts
        Left err -> print err

