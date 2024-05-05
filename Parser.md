```haskell
many1 p = (:) <$> p <*> many p
many p = many1 p <|> pure []
```