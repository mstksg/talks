% Practical Dependent Types: Type-Safe Neural Networks
% Justin Le https://blog.jle.im (justin@jle.im)
% Lambdaconf 2017, May 27, 2017

## Preface

Slide available at
<https://mstksg.github.io/talks/lambdaconf-2017/dependent-types/dependent-types.html>.

Exercises we will be doing available at 
<https://github.com/mstksg/talks/tree/master/lambdaconf-2017/dependent-types>.

Libraries required: (available on Hackage)
-   *hmatrix*
-   *singletons*
-   *MonadRandom*

GHC 8.x assumed.

## The Big Question

The big question of Haskell: *What can types do for us?*

. . .

Dependent types are simply the extension of this question, pushing the power of
types further.

## Artificial Neural Networks

![Feed-forward ANN architecture](img/ffneural.png "Feed-forward ANN architecture")

## Parameterized functions

Each layer receives an input vector, $\mathbf{x} : \mathbb{R}^n$, and produces
an output $\mathbf{y} : \mathbb{R}^m$.

. . .

They are parameterized by a weight matrix $W : \mathbb{R}^{m \times n}$ (an $m
\times n$ matrix) and a bias vector $\mathbf{b} : \mathbb{R}^m$, and the result
is:

$$
\mathbf{y} = f( W \mathbf{x} + \mathbf{b})
$$

Where $f$ is some (differentiable) activation function.

A neural network would take a vector through many layers.

## Networks in Haskell

```haskell
data Weights = W { wBiases :: !(Vector Double)  -- n
                 , wNodes  :: !(Matrix Double)  -- n x m
                 }                              -- "m to n" layer


data Network :: Type where
    O    :: !Weights -> Network
    (:~) :: !Weights -> !Network -> Network
infixr 5 :~
```

. . .

A network with one input layer, two hidden layers, and one output layer would
be:

```haskell
h1 :~ h2 :~ O o
````

## Running them

```haskell
runLayer :: Weights -> Vector Double -> Vector Double
runLayer (W wB wN) v = wB + wN #> v

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)     !v = logistic (runLayer w v)
runNet (w :~ n') !v = let v' = logistic (runLayer w v)
                      in  runNet n' v'
```

## Generating them

```haskell
randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
    seed1 :: Int <- getRandom
    seed2 :: Int <- getRandom
    let wB = randomVector  seed1 Uniform o * 2 - 1
        wN = uniformSample seed2 o (replicate i (-1, 1))
    return $ W wB wN

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i []     o =    O <$> randomWeights i o
randomNet i (h:hs) o = (:~) <$> randomWeights i h <*> randomNet h hs o
```

## Haskell Heart Attacks

> - What if we mixed up the dimensions for `randomWeights`?
> - What if layers in the network are incompatible?
> - How does the user know what size vector a network expects?
> - Is our `runLayer` and `runNet` implementation correct?

## Backprop

```haskell
train :: Double           -- ^ learning rate
      -> Vector Double    -- ^ input vector
      -> Vector Double    -- ^ target vector
      -> Network          -- ^ network to train
      -> Network
train rate x0 target = fst . go x0
  where
```

## Backprop (Outer layer)

```haskell
    go :: Vector Double    -- ^ input vector
       -> Network          -- ^ network to train
       -> (Network, Vector Double)
    -- handle the output layer
    go !x (O w@(W wB wN))
        = let y    = runLayer w x
              o    = logistic y
              -- the gradient (how much y affects the error)
              --   (logistic' is the derivative of logistic)
              dEdy = logistic' y * (o - target)
              -- new bias weights and node weights
              wB'  = wB - scale rate dEdy
              wN'  = wN - scale rate (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (O w', dWs)
```

## Backprop (Inner layer)

```haskell
    -- handle the inner layers
    go !x (w@(W wB wN) :~ n)
        = let y          = runLayer w x
              o          = logistic y
              -- get dWs', bundle of derivatives from rest of the net
              (n', dWs') = go o n
              -- the gradient (how much y affects the error)
              dEdy       = logistic' y * dWs'
              -- new bias weights and node weights
              wB'  = wB - scale rate dEdy
              wN'  = wN - scale rate (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (w' :~ n', dWs)
```

## Compiler, O Where Art Thou?

> - Haskell is all about the compiler helping guide you write your code.  But how  much did the compiler help there?
> - How can the "shape" of the matrices guide our programming?

## Haskell Red Flags

> - How many ways can we write the function and have it still typecheck?
> - How many of our functions are partial?

## A Typed Alternative

```haskell
data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }
```

An `o x i` layer

## A Typed Alternative

From HMatrix:

```haskell
R :: Nat -> Type
L :: Nat -> Nat -> Type
```

An `R 3` is a 3-vector, an `L 4 3` is a 4 x 3 matrix.

. . .

Operations are typed:

```haskell
(+)  :: KnownNat n               => R n   -> R n -> R n
(<#) :: (KnownNat m, KnownNat n) => L m n -> R n -> R m
```

(`KnownNat n` lets hmatrix use the `n` in the type)

## Data Kinds

With `-XDataKinds`, all values and types are lifted to types and kinds.

. . .

In addition to the values `True`, `False`, and the type `Bool`, we also have
the **type** `'True`, `'False`, and the **kind** `Bool`.

In addition to `:` and `[]` and the list type, we have `':` and `'[]` and the
list kind.

## Data Kinds

```haskell
ghci> :t True
Bool
ghci> :k 'True
Bool
ghci> :t [True, False]
[Bool]
ghci> :k '[ 'True, 'False ]
[Bool]
```

## A Typed Alternative

```haskell
data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:~) :: KnownNat h
         => !(Weights i h)
         -> !(Network h hs o)
         -> Network i (h ': hs) o
infixr 5 :~
```

. . .

```haskell
h1 :: W 10 8
h2 :: W 8  5
o  :: W 5  2

h1 :~ h2 :~ o :: Network 10 '[8, 5] 2

h2 :~ h1 :~ o -- type error
```

## Running

```haskell
runLayer :: Weights -> Vector Double -> Vector Double
runLayer (W wB wN) v = wB + wN #> v

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)     !v = logistic (runLayer w v)
runNet (w :~ n') !v = let v' = logistic (runLayer w v)
                      in  runNet n' v'
```

## Running

Much better!  Matrices and vector lengths are guaranteed to line up!

## Generating

```haskell
randomWeights :: (MonadRandom m, KnownNat i, KnownNat o)
              => m (Weights i o)
randomWeights = do
    s1 :: Int <- getRandom
    s2 :: Int <- getRandom
    let wB = randomVector  s1 Uniform * 2 - 1
        wN = uniformSample s2 (-1) 1
    return $ W wB wN
```

No need for explicit arguments!

## Generating

But, for generating nets, we have a problem:

```
randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNat o)
          => m (Network i hs o)
randomNet = case hs of [] -> ??
```

## Pattern matching on types

The solution for pattern matching on types: singletons.

```haskell
data Sing :: Bool -> Type where
    SFalse :: Sing 'False
    STrue  :: Sing 'True

data Sing :: [k] -> Type where
    SNil  :: Sing '[]
    SCons :: Sing x -> Sing xs -> Sing (x ': xs)

data Sing :: Nat -> Type where
    SNat :: KnownNat n => Sing n
```

## Pattern matching on types

```haskell
ghci> :t SFalse
Sing 'False
ghci> :t STrue `SCons` (SFalse `SCons` SNil)
Sing '[True, False]
ghci> :t SNat @1 `SCons` (SNat @1 `SCons` SNil)
Sing '[1, 2]
```

## Random networks

```haskell
randomNet' :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNat o)
           => Sing hs -> m (Network i hs o)
randomNet' = \case
    SNil            ->    O <$> randomWeights
    SNat `SCons` ss -> (:~) <$> randomWeights <*> randomNet' ss

```

## Implicit passing


```haskell
class SingI x where
    sing :: Sing x
```

```haskell
randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, SingI hs, KnownNat o)
          => m (Network i hs o)
randomNet = randomNet' sing
```

## Type-Driven Development

We wrote an untyped implementation, then realized what was wrong.  Then we
added types!

## Further reading

Blog series:
<https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html>