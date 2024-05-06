```haskell
many1 p = (:) <$> p <*> many p
many p = many1 p <|> pure []
```

getZipList :: ZipList a -> [a]

----------------------------
```
runState (return 'X') 1
return :: State Int Char
```
return set the result value but leave the state unchanged

```
runState get 1 
get :: State Int Int 
```
get set the result value to the state and leave the state unchanged

```
runState (put 5) 1
put 5 :: State Int () 
```
put set the result value to () and set the state value