module Main

import Data.Vect

%default total

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
-- addToStore datastore item = MkData (size datastore + 1) (items datastore ++ [item])
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> (inp : String) -> Maybe (String, DataStore)
getEntry pos store inp
  = let store_items = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("Out of range\n", store)
             Just id => Just (index id store_items ++ "\n", store)

getMatches : (str : String) -> (current_index : Nat) -> (store_items : Vect n String) -> String
getMatches str current_index [] = "\n"
getMatches str current_index (item :: items) = if isInfixOf str item
                                                  then show current_index ++ ": " ++ item ++ "\n" ++ getMatches str (current_index + 1) items
                                                  else getMatches str (current_index + 1) items

{-
enumerate : (n : Nat) -> Vect n Nat
enumerate Z = []
enumerate (S k) = enumerate k ++ [k]
-}

searchEntry : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry str store
  = let store_items = items store in
        Just (getMatches str 0 store_items, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store inp
         Just Size => Just ("The size of the data store is " ++ show (size store) ++ "\n", store)
         Just (Search str) => searchEntry str store
         Just Quit => Nothing

partial
main : IO ()
main = replWith (MkData _ []) "Command: " processInput
