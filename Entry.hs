module Entry where

data Person = Person { name :: String, 
		       address :: String,
                       nickname :: String}

instance Show Person where
    show x = name x ++ ", " ++ address x

updateName :: Person -> String -> Person
updateName p newName = p {name = newName}

updateAddress :: Person -> String -> Person
updateAddress p newAddr = p {address = newAddr} 
