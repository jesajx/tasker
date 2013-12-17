
module Tasker
( Task(..)
, name
, description
, parseTaskFile
) where

import Text.ParserCombinators.Parsec
import Data.Functor ((<$>))
import Data.List (intercalate)

import System.IO ( stdin, stdout
                 , openFile, hClose
                 , IOMode(ReadMode, WriteMode)
                 , hGetContents, hPutStrLn
                 )
import System.Environment (getArgs)
import Control.Monad (when)

data Task = Task String String
          | TaskGroup String [Task]
          deriving Show

name :: Task -> String
name (Task name _) = name

description :: Task -> String
description (Task _ description) = description

normalize :: [Task] -> [Task]
normalize ts = concatMap (normalize' "") ts
    where normalize' p (Task s d) = [Task (newName p s) d]
          normalize' p (TaskGroup s t) =
              concatMap (normalize' $ newName p s) t
          newName "" x = x
          newName p x = p ++ '.':x

------

-- parseTaskFile = items
-- items = (spaces*, item, spaces*)*
-- item = task | group
-- task = "task", ' '+, name, (',', ' '+, name)*, description?, '\n'
-- description = ' '+, ": ", -'\n'+
-- group = "group", ' '+, name, (',', ' '+, name)*, groupCont?, '\n'
-- groupCont = '{', items, spaces*, '}'
-- name = %alpha


parseTaskFile :: String -> Either ParseError [Task]
parseTaskFile s = parse p "" s
    where p = do
            many sl
            ts <- items
            eof
            return ts

--items = return . concat =<< option [] (many $ between spaces spaces item)
items = do
    ts <- many $ do
        t <- item
        many sl
        return t
    return $ concat ts
    

item = task <|> group

task = do
    ns <- typeAndName "task"
    d <- option "" descriptionParser
    newline
    return $ map (`Task` d) ns

group = do
    ns <- typeAndName "group"
    ts <- option [] groupContent
    newline
    return $ map (`TaskGroup` ts) ns
    
names = sepBy1 cont sep
    where cont = do
            s <- nameParser
            many sp
            return s
          sep = do
            char ','
            many sp
    
sp = char ' '
sl = oneOf " \n"


nameParser = many1 $ noneOf " ,\n:"

descriptionParser = do
    many sp
    string ": "
    manyTill anyChar $ try newline

    
groupContent = do
    char '{'
    many sl
    t <- items
    char '}'
    return t

typeAndName s = do
    string s
    many1 sp
    names



----

-- TODO
-- read file
-- normalize
-- priority
-- deadline
-- generalize description?
-- 
main = do
    args <- getArgs
    cont <- readFile $ args !! 0
    let et = parseTaskFile cont
    let t = case et of Right k -> k -- TODO where is 'fromRight'?
    putStrLn "#tasks/groups:"
    mapM_ print t 
    putStrLn "#normalized:"
    mapM_ print $ normalize t 
