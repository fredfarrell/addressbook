import System.Environment
import Database.HDBC.Sqlite3
import Database.HDBC

import AddrBookInterface
import Entry

main = do
       conn <- connectSqlite3 "test1.db" 
       args <- getArgs

       case args of
           ["get", name] -> get conn name
           ["add", name] -> add conn name
           _ -> commandError

       disconnect conn

get :: Connection -> String -> IO ()
get conn name = do
                res <- getAddressByNickname conn "addressbook" name
                putStr $ showPeople (entriesToPeople res)

add :: Connection -> String -> IO ()
add conn name = do 
                putStrLn "Enter person's full name:"
                fullname <- getLine
                putStrLn "Enter person's address:"
		address <- getLine
                
                let p = Person {name = fullname, address = address, nickname = name}

                success <- addEntry conn "addressbook" p

                case success of
                    1 -> putStrLn "Entry added."
                    0 -> putStrLn "Failed to connect to database."

                commit conn

commandError = do 
               putStrLn "Usage: address command [args]"
               putStrLn ""
               putStrLn "address add name   Adds an entry for the given name."
               putStrLn "address get name   Gets full name and address of the person with the given name."
