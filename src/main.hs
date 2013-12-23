

import Tasker
import System.IO ( stdin, stdout
                 , openFile, hClose
                 , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)
import Data.List (sortBy, intercalate)



-- TODO ops

taskStr (Task n dt d deps) = "task " ++ n ++ dtStr dt ++ depStr deps ++ dStr d
    where dtStr Nothing = ""
          dtStr (Just k) = " [" ++ show k ++ "]"
          depStr [] = ""
          depStr xs = " {" ++ intercalate ", " deps ++ "}"
          dStr "" = ""
          dStr k = " : \"" ++ k ++ "\""
            

main = do
    args <- getArgs
    let arg1 = args !! 0
    cont <- readFile arg1
    case parse taskParser arg1 cont of
        Right ts -> do
            -- mapM_ print ts
            mapM_ (putStrLn . taskStr) $ sortBy cmpDeadline $ refine ts
        Left err -> print err

