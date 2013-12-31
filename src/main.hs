

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
    let action = args !! 0
    let filename = args !! 1
    cont <- readFile filename
    case parse taskParser filename cont of
        Right ts ->
            let ss = L.sort $ expandRaw ts in
            case action of
                "raw"  -> mapM_ print ts
                "ls"   -> mapM_ print $ sortTasks ss
        Left err -> print err

