module AddrBookInterface where

import Database.HDBC
import Database.HDBC.Sqlite3
import Entry
import Data.Maybe (catMaybes)

addEntry :: IConnection conn => conn -> String ->  Person -> IO Integer
addEntry conn table person = run conn query [nm, addr, nick]
                           where query = "INSERT INTO " ++ table ++ " VALUES (?,?,?)"
                                 nm = toSql $ name person
                                 addr = toSql $ address person
                                 nick = toSql $ nickname person

getAddressByNickname :: IConnection conn => conn -> String -> String -> IO [[SqlValue]]
getAddressByNickname conn table nick = quickQuery conn query [toSql nick]
                           where query = "SELECT * FROM " ++ table ++ " WHERE nickname = ?"  

entryToPerson :: [SqlValue] -> Maybe Person
entryToPerson [sqlName, sqlAddr, sqlNick] = Just $ Person {name = fromSql sqlName, address = fromSql sqlAddr, nickname = fromSql sqlNick}
entryToPerson _ = Nothing

entriesToPeople :: [[SqlValue]] -> [Maybe Person]
entriesToPeople xs = [entryToPerson x | x <- xs]

showPeople :: [Maybe Person] -> String
showPeople = unlines . (map show) . catMaybes
