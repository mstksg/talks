---
title: Applicative Regular Expressions w/ the Free Alternative
theme: sky
author: Justin Le <https://blog.jle.im>
date: C◦mp◦se 2019, June 24
...

## Preface

Slide available at <https://talks.jle.im>.

## Regular Expressions


:::::: {.columns}

:::{.column}

```haskell
(a|b)(cd)*e
```
:::

:::{.column}

![](img/regexp.png "Structure")
:::
::::::

. . .

:::::: {.columns}

:::{.column}

Matches:

*   ae
*   acdcdcde
*   bcde
:::

:::{.column}

Doesn't match:

*   acdcd
*   abcde
*   bce
:::
::::::

## "Captures"


:::::: {.columns}

:::{.column}

```haskell
(a|b)(cd)*e
```
:::

:::{.column}

![](img/regexp.png "Structure")
:::
::::::

. . .

::::::::: {.columns}

:::{.column}

*   ae $\rightarrow$
*   acdcdcde $\rightarrow$
*   bcde $\rightarrow$
:::

::::::{.column}

::: incremental

*   `("a","")`
*   `("a","cdcdcd")`
*   `("b","cd")`
:::
::::::
:::::::::

## Applicative Regular Expressions

"Type-indexed" regular expressions.

```haskell
type Regexp a
         -- ^ type of "result"
```

. . .

```haskell
char   :: Char   -> RegExp Char
string :: String -> RegExp String
```

. . .

```haskell
runRegexp :: RegExp a -> String -> Maybe a
```

## Applicative Regular Expressions

```haskell
char   :: Char     -> RegExp Char
string :: String   -> RegExp String
(<|>)  :: RegExp a -> RegExp a -> RegExp a
many   :: RegExp a -> RegExp [a]

myRegexp :: RegExp (Char, [String])
myRegexp = (,) <$> (char 'a' <|> char 'b')
               <*> many (string "cd")
               <*  char 'e'
```

. . .

```haskell
runRegexp myRegexp :: String -> Maybe (Char, [String])
```

## Applicative Regular Expressions

```haskell
runRegexp myRegexp "ae"
Just ('a', [])

runRegexp myRegexp "acdcdcde"
Just ('a', ["cd","cd","cd"])

runRegexp myRegexp "bcde"
Just ('b', ["cd"])

runRegexp myRegexp "acdcd"
Nothing
```

## Applicative Regular Expressions

```haskell
myRegexp2 :: RegExp (Bool, Int)
myRegexp2 = (,) <$> ((False <$ char 'a') <|> (True <$ char 'b'))
                <*> fmap lengnth (many (string "cd"))
                <*  char 'e'
```

. . .

```haskell
runRegexp myRegexp2 "ae"
Just (False, 0)

runRegexp myRegexp2 "acdcdcde"
Just (False, 3)

runRegexp myRegexp2 "bcde"
Just (True, 1)
```

## What's so Regular?

. . .

**Regular Language Base Members**

1.  Empty set: Always fails to match
2.  Empty string: Always succeeds, consumes nothing
3.  Literal: Matches and consumes a given char

. . .

**Regular Language Operations**

1.  Concatenation: `RS`, sequence one after the other
2.  Alternation: `R|S`, one or the other
3.  Kleene Star: `R*`, the repetition of `R`

## An Alternative Perspective

. . .

```haskell
class Functor f => Applicative f where
    -- | Always succeed, consuming nothing
    pure  :: a -> f a
    -- | Concatenation
    (<*>) :: f (a -> b) -> f a -> f b
```

. . .

```haskell
class Applicative f => Alternative f where
    -- | Always fails to match
    empty :: f a
    -- | Alternation
    (<|>) :: f a -> f a -> f a
    -- | Reptition
    many  :: f a -> f [a]
```

## An Alternative Perspective


::: incremental

1.  Empty set: `empty`
2.  Empty string: `pure x`
3.  Literal: ???
4.  Concatenation: `<*>`
5.  Alternation: `<|>`
6.  Repetition: `many`

:::

## Functor combinator-style


::: incremental

*   Define a primitive type

    ```haskell
    type Prim a
    ```
*   Add the structure you need
    *   If this structure is from a typeclass, use the free structure of that
        typeclass
:::

## Easy as 1, 2, 3

```haskell
data Prim a = Prim Char a
  deriving Functor

data Alt :: (Type -> Type) -> (Type -> Type)
          -- ^ take a Functor
                            -- ^ return a Functor

type RegExp = Alt Prim

liftAlt :: Prim a -> Alt Prim

char :: Char -> RegExp Char
char c = liftAlt (Prim c c)
```

## Unlimited Power

```haskell
empty :: RegExp a
pure  :: a -> RegExp a
char  :: Char -> RegExp Char
(<*>) :: RegExp (a -> b) -> RegExp a -> RegExp b
(<|>) :: RegExp a -> RegExp a -> RegExp a
many  :: RegExp a -> RegExp [a]
```

. . .

```haskell
string :: String -> RegExp String
string = traverse char

digit :: RegExp Int
digit = asum [ intToDigit i <$ char i | i <- [0..9] ]
```

## Parsing

Options:

::: incremental

1.  *Interpret* into an `Alternative` instance, "offloading" the logic
2.  Direct pattern match on structure constructors (Haskell 101)
:::

## What is freeness?

. . .

```haskell
type FreeMonoid = []

injectFM :: a -> FreeMonoid a
runFM    :: Monoid m => (a -> m) -> (FreeMonoid a -> m)
```

. . .

```haskell
(:[])   :: a -> [a]
foldMap :: Monoid m => (a -> m) -> ([a] -> m)
```

## What is freeness?

```haskell
myMon :: FreeMonoid Int
myMon = [1] <> [2] <> [3] <> [4]
```

. . .

```haskell
foldMap Sum myMon
Sum 10
```

. . .

```haskell
foldMap Product myMon
Product 24
```

. . .

```haskell
foldMap Max myMon
Max 4
```

## What is freeness?

```haskell
type Alt a

liftAlt :: f a -> Alt f a
runAlt  :: Alternative g
        => (forall b. f a -> g a)
        -> (Alt f a -> g a)
```

## Hijacking StateT

```haskell
StateT [Char] Maybe
```

*   `Prim a` can be interpreted as *consumption*
*   `<*>` sequences consumption
*   `<|>` is backtracking

## Hijacking StateT

```haskell
processPrim :: Prim a -> StateT String Maybe a
processPrim (Prim c x) = do
    d:ds <- get             -- ^ match on stream
    guard (c == d)          -- ^ fail unless match
    put ds                  -- ^ update stream
    return x                -- ^ return result value

matchPrefix :: Regexp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)
```

## This works?

. . .

Yes!

```haskell
matchPrefix myRegexp2 "ae"
Just (False, 0)

matchPrefix myRegexp2 "acdcdcde"
Just (False, 3)

matchPrefix myRegexp2 "bcde"
Just (True, 1)
```

## What just happened?

. . .

```haskell
data Prim a = Prim Char a
  deriving Functor

type RegExp = Alt Prim

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)
  where
    processPrim (Prim c x) = do
      d:ds <- get
      guard (c == d)
      put ds
      pure x
```

## What just happened?


::: incremental

1.  Offload `Alternative` functionality to `StateT`: `empty`, `<*>`, `pure`,
    `empty`, `many`.
2.  Provide `Prim`-processing functionality with `processPrim`: `liftAlt`.
:::

## What do we gain?

::: incremental

1.  Interpretation-invariant structure
2.  Actually meaningful types
:::

## What do we gain

`StateT String Maybe` is **not** a regular expression type.

```haskell
notARegexp :: StateT String Maybe ()
notARegexp = put "hello"        -- no regular expression
```

. . .

`Alt Prim` **is** a regular expression type

## Direct matching

```haskell
newtype Alt f a = Alt { alternatives :: [AltF f a] }

data AltF f a = forall r. Ap (f r) (Alt f (r -> a))
              |           Pure a
```

. . .

```haskell
-- | Chain of <|>s
newtype Alt f a
    =           Choice (AltF f a) (Alt f a       )  -- ^ cons
    |           Empty                               -- ^ nil

-- | Chain of <*>s
data AltF f a
    = forall r. Ap     (f r     ) (Alt f (r -> a))  -- ^ cons
    |           Pure a                              -- ^ nil
```

## Direct Matching

```haskell
matchAlts :: RegExp a -> String -> Maybe a
matchAlts (Alt res) xs = asum [ matchChain re xs | re <- res ]
```

. . .

```haskell
matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) cs = _
matchChain (Pure x)             cs = _
```

## One game of Type Tetris later...

```haskell
matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) cs = case cs of
    []  -> Nothing
    d:ds | c == d    -> matchAlts (($ x) <$> next) ds
         | otherwise -> Nothing
matchChain (Pure x)             _      = Just x
```

## This works?

. . .

Yes!

```haskell
matchChain myRegexp2 "ae"
Just (False, 0)

matchChain myRegexp2 "acdcdcde"
Just (False, 3)

matchChain myRegexp2 "bcde"
Just (True, 1)
```

## What do we gain?

::: incremental

*   First-class program rewriting, Haskell 101-style
*   **Normalizing** representation
    *   Equivalence in meaning = equivalence in structure
:::

. . .

```haskell
-- | Not regularizing
data RegExp a = Empty
              | Pure a
              | Prim Char a
              | forall r. Seq (RegExp a) (RegExp (r -> a))
              | Union (RegExp a) (RegExp a)
              | Many (RegExp a)

-- a|(b|c) /= (a|b)|c
```

## Free your mind

Is this you?

> "My problem is modeled by some (commonly occurring) structure over some
> primitive base."

::: incremental

*   Use a "functor combinator"!
*   If your structure comes from a typeclass, use a free structure!
:::

## Further Reading

*   Blog post: <https://blog.jle.im/entry/free-applicative-regexp.html>
*   Functor Combinatorpedia: <https://blog.jle.im/entry/functor-combinatorpedia.html>
*   Slides: <https://talks.jle.im/composeconf-2019/free-alternative.md>
