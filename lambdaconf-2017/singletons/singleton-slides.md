% Singletons and You
% Justin Le
% Lambdaconf 2017, May 27, 2017

## Singletons

```haskell
toSomeVec :: [a] -> SomeVec a
toSomeVec []     = SomeVec SZ VNil
toSomeVec (x:xs) = case toSomeVec xs of
    SomeVec s xs' -> SomeVec (SS s) (x :* xs')
```

