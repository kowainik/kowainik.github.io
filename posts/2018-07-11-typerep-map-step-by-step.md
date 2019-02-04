---
title: "typerep-map step by step"
author: Veronika Romashkina
tags: haskell, dependent types, typerep, data structure, GADTs
description: "Efficient implementation of Map with types as keys"
---

Recently, I have been working on a very interesting and sophisticated project
called [`typerep-map`](https://hackage.haskell.org/package/typerep-map).
A lot of advanced features and tricks were used during the development process
and I have discovered many amusing and new sides of Haskell. So, I decided to
share the ideas, steps, issues, etc. in this blog post.

If you want to skip all the funny parts, here is the link to the code itself:

 * [kowainik/typerep-map](https://github.com/kowainik/typerep-map)

## What it’s all about

The basic idea behind `typerep-map` is to have a data structure like
[`Map`](http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#t:Map),
but where types serve as keys, and values stored in the map are of the type
specified in the corresponding key.

An example image of this structure:

```yaml
Int: 42
Bool: True
String: "Haskell"
```

There can be only one key-value pair for each type.

And here is an example written in pseudo-code for better understanding:

```haskell
insert (42 :: Int) empty = [(Int, 42)]
                             ^     ^
                            key   value

insert (42 :: Int) (insert True empty) = [(Int, 42), (Bool, True)]
                                           ^     ^      ^     ^
                                          key   value  key  value
```

We also want our values to be indexed by a polymorphic type, but that will be
explained later.

### Existing Solutions

There already exist some libraries that implement ideas similar to `typerep-map`:

* [`type-map`](https://hackage.haskell.org/package/type-map) appears to resemble
  our project, however the interface is different. They track the elements in
  the types and don't provide the desired parametrization.
* [`dependent-map`](https://hackage.haskell.org/package/dependent-map) is closer
  to our goal in terms of the interface but the package has a complete
  reimplementation of `Data.Map.Lazy`, and the goal of the `typerep-map` project
  is to have an efficient implementation based on primitive unboxed arrays.

### Motivation

You might wonder what `typerep-map` brings to the table if there are other
packages that aim to fulfil the same purpose. The primary goal is to use it in
the [`caps`](https://github.com/int-index/caps) library instead of the
[`DMap`](https://hackage.haskell.org/package/dependent-map-0.2.4.0/docs/Data-Dependent-Map.html#t:DMap)
type from `dependent-map` parametrized by `TypeRep`.

In `caps` the performance of lookups is extremely important so it makes sense to
prioritize its performance above that of other functions.

## Implementing TypeRepMap

Sections below describe the details of the implementation phases and the general
concepts.

> __NOTE:__ in this blog post I am talking about `ghc-8.0.2` or higher.

### Extensions used

The code snippets in this blog post assume that the following extensions are enabled:

 * `-XTypeApplications`
 * `-XScopedTypeVariables`
 * `-XGADTs`
 * `-XTypeInType`
 * `-XAllowAmbiguousTypes`


## Map-based implementation

The reference implementation is more or less straightforward. It uses a lazy
[`Map`](http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#t:Map)
from `containers` as an internal representation of the desired data type.

### Keys

Normally, types in Haskell are only present at compile-time: after
type-checking, they are completely erased from the program. And yet we want to
use types as keys in a map. This requires a runtime representation for types.
Luckily, the standard library provides a runtime representation for types in the
form of
[`TypeRep`](https://hackage.haskell.org/package/base/docs/Data-Typeable.html#t:TypeRep).
But there are actually two different definitions of `TypeRep` in `base`:

* [Type.Reflection.TypeRep](https://hackage.haskell.org/package/base/docs/Type-Reflection.html#t:TypeRep)
* [Data.Typeable.TypeRep](https://hackage.haskell.org/package/base/docs/Data-Typeable.html#t:TypeRep)

The one in `Type.Reflection` was introduced in GHC 8.2 and the old one was
rewritten to be based on it. `Type.Reflection.TypeRep` has kind `TypeRep :: k -> *`
while the old one has kind `TypeRep :: *`.

To have the basic idea of what actually `TypeRep` is, you can think of the old
`TypeRep` as an infinite ADT with all types enumerated as tag constructors:

 ```haskell
 data TypeRep = Int | Bool | Char | String | ...
 ```

and the new `TypeRep` is an equivalent to the infinite
[GADT](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts):

```haskell
 data TypeRep (a :: k) where
     Int    :: TypeRep Int
     Bool   :: TypeRep Bool
     Char   :: TypeRep Char
     String :: TypeRep String
     ...
 ```

If you are interested in the actual difference between old and new versions of
the `TypeRep` and motivation for this change, here is a nice ICFP video by Simon
Peyton Jones:

  * [ICFP 2015: Lightning: New Typeable](https://www.youtube.com/watch?v=RPoqCbaX6og)

I use the old `TypeRep` that comes from `Data.Typeable`. And I have an
explanation for that: there is a limitation in regular `Map` that all keys must
be of the same type and this is not possible to achieve with parameterized
`TypeRep`. Also, the old `TypeRep` will never be deprecated (from `8.2` it is
just a different interface to the new `TypeRep`, so it's not obsolete), and it
is sufficient for our goal to support older GHC versions.

Here is a usage example of basic `TypeRep` interface:

```haskell
ghci> :t typeRep
typeRep :: Typeable a => proxy a -> TypeRep

ghci> typeRep (Proxy @Int)
Int

ghci> :t it
it :: TypeRep
```

### Values

For the first prototype, I decided to use
[`Dynamic`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Dynamic.html#t:Dynamic)
as values in our `TypeRepMap`.

```haskell
ghci> :t toDyn
toDyn :: Typeable a => a -> Dynamic

ghci> toDyn True
<<Bool>>

ghci> fromDynamic (toDyn "Haskell") :: Maybe String
Just "Haskell"
```

So we've got:

```haskell
newtype TypeRepMap = TypeRepMap { unMap :: Map TypeRep Dynamic }
```

and the initial interface looks like this:

```haskell
insert :: forall a . Typeable a => a -> TypeRepMap -> TypeRepMap
insert val = TypeRepMap . LMap.insert (typeRep (Proxy @a)) (toDyn val) . unMap

lookup :: forall a . Typeable a => TypeRepMap -> Maybe a
lookup = fromDynamic <=< LMap.lookup (typeRep $ Proxy @a) . unMap
```

When looking at the
[`Dynamic`](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Dynamic.html#Dynamic)
data type implementation

```haskell
data Dynamic = Dynamic TypeRep Obj
type Obj = Any
```

you can notice that it already stores `TypeRep` inside, so it seems like it's a
bit suboptimal decision due to redundancy. And we can safely use
[`Any`](https://hackage.haskell.org/package/ghc-prim-0.5.2.0/docs/src/GHC.Types.html#Any)
as our value type.

According to the `Dynamic` implementation, we can use
[`unsafeCoerce`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Unsafe-Coerce.html#v:unsafeCoerce)
function for the conversion to `Any` and from `Any`.

So we get:

```haskell
newtype TypeRepMap = TypeRepMap { unMap :: LMap.Map TypeRep Any }

insert val =
      TypeRepMap
    . LMap.insert (typeRep $ Proxy @a) (unsafeCoerce val)
    . unMap

lookup = fmap unsafeCoerce . LMap.lookup (typeRep $ Proxy @a) . unMap

```

Let's check how it's all working:

```haskell
ghci> let x = lookup $ insert (11 :: Int) empty

ghci> x :: Maybe Int
Just 11

ghci> x :: Maybe ()
Nothing
```

All right, we have a simple working version. But there are ways to improve it.

### Parameterization

The next step is to parametrize our data type by type variable `f` with kind
`f :: k -> *`. This `f` will be the _interpretation_ of our keys. Such
parameterization allows us to encode additional structure common between all
elements, making it possible to use `TypeRepMap` to model a variety of things
from extensible records to monadic effects. This sort of parametrization may be
familiar to users of [`vinyl`](http://hackage.haskell.org/package/vinyl)
records.

Note that the input kind is `k` — we want to support arbitrary kinds as well.
Since `TypeRep` is poly-kinded, the interpretation can use any kind for the keys
(see some examples in
[documentation](https://hackage.haskell.org/package/typerep-map-0.1.0/docs/Data-TypeRepMap.html)).

```haskell
newtype TypeRepMap (f :: k -> *) = TypeRepMap
    { unMap :: LMap.Map TypeRep Any
    }
```

The implementation of the functions stays the same, but the types are different:

```haskell
insert :: forall a f . Typeable a => f a -> TypeRepMap f -> TypeRepMap f
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
```

Our previous implementation is just equivalent to `TypeRepMap Identity` in the
context of the described design.

> **NOTE:** Another reason to get rid of the `Dynamic`: if we keep it then we
> have to specify `Typeable (f a)` constraint instead of `Typeable a` in the
> type declarations. And having `Typeable a` constraint would let us implement
> the following function efficiently:
>
> ```haskell
> hoist :: (forall x. f x -> g x) -> TypeRepMap f -> TypeRepMap g
> ```

## Vector-based implementation

The next step is to write an alternative implementation based on
[unboxed vectors](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed.html),
which is expected to be faster.

We want to use `Vector (TypeRep, Any)` instead of our lazy map. This vector is
going to be sorted by `TypeRep`. `insert`/`lookup` algorithms should be
implemented manually in the following way:

* `insert`: allocate a new vector of `n + 1` element, copy everything from the initial vector adding the new element and don't forget to keep the sorting.
* `lookup`: the simple binary search.

The point of the unboxed vector is that it helps to get rid of the pointer
indirection. If we take just `Vector` we will observe this picture (`Ty` stands
for `TypeRep` and `El` stands for an element):

```haskell
  [   Pair₁,      Pair₂,      Pair₃,      Pair₄   ]
      /   \       /   \       /   \       /   \
    Ty₁   El₁   Ty₂   El₂   Ty₃   El₃   Ty₄   El₄
```

Instead of this what we would like to see is:

```haskell
    [ Ty₁, El₁, Ty₂, El₂, Ty₃, El₃, Ty₄, El₄ ]
```

In this way, as the result, the access to the `Ty` or `El` is shorter for
exactly one pointer dereference.

However, turned out that it's more efficient to store keys and values in
separate vectors under corresponding indices:

```haskell
    [ Ty₁,   Ty₂,    Ty₃,    Ty₄ ]
       |      |       |       |
    [ El₁,   El₂,    El₃,    El₄ ]
```

Unfortunately, `TypeRep` doesn't have the
[`Unbox`](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed.html#t:Unbox)
instance and it looks like it's not possible to write it. So instead of storing
`TypeRep` we will be storing a
[`Fingerprint`](https://hackage.haskell.org/package/base-4.11.1.0/docs/GHC-Fingerprint.html#t:Fingerprint).
Basically, `Fingerprint` is the hash for `TypeRep`, so it makes sense to move in
this direction.

```haskell
ghci> :t typeRepFingerprint
typeRepFingerprint :: TypeRep -> Fingerprint

ghci> typeRepFingerprint $ typeRep $ Proxy @Int
b1460030427ac0fa458cbf347f168b53

ghci> typeRepFingerprint $ typeRep $ Proxy @Bool
ebf3a8541b05453b8bcac4a38e8b80a4
```

`TypeRep` from `Data.Typeable` module is defined as

```haskell
type TypeRep = SomeTypeRep
```

If we take a look at the [`Ord` instance](https://hackage.haskell.org/package/base-4.11.1.0/docs/src/Data.Typeable.Internal.html#line-318)
of `SomeTypeRep` in `base` we'll see that it's confirmed that `Fingerprint` is
unique for each `TypeRep`. That means it's okay to use `Fingerprint` as a key
instead of `TypeRep`.

### Vector

This is initial vector-based implementation:

```haskell
data TypeRepMap (f :: k -> *) = TypeRepMap
    { fingerprints :: Vector Fingerprint
    , anys         :: Vector Any
    }
```

We want to use [unboxed vector](https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html#t:Vector)
as a type for the `fingerprints` field of `TypeRepMap`.

Every unboxed vector is the newtype wrapper over some primitive vector. In order
to use an unboxed vector of `Fingerprint` we need to implement an instance of the
`Prim` typeclass from the
[`primitive`](https://hackage.haskell.org/package/primitive) package for
`Fingerprint`. It was proposed to add this instance under this issue in
`primitive` library (having this instance inside library would simplify
implementation a lot):

* [Prim instance for Fingerprint](https://github.com/haskell/primitive/issues/70)

As the reference for `Prim` instance implementation, we can use the
[`Storable`](https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Storable.html#t:Storable)
type class which contains similar functions. There is already the instance
`Storable` for `Fingerprint`. An assumption is that there is no significant
difference between `Storable` and `Prim` for our `lookup` checks and we can use
storable vector instead of unboxed one. For more information about the
difference between those typeclasses see
[this SO answer](https://stackoverflow.com/questions/40176678/differences-between-storable-and-unboxed-vectors/40198950#40198950).

Though our initial assumptions were false and turned out that `Storable` doesn't
give the desired performance boost as shown with benchmarks.

### Optimal Vector

According to the source code, `Fingerprint` is a pair of `(Word64, Word64)`. So
instead of having a single vector of `Fingerprint`s we can have a vector of
`Word64` where `Fingerprint` with number `i` stored on `2 * i` and `2 * i + 1`
indices.

But actually, it’s better to split it into two separate vectors of `Word64`
where one vector stores the first halves of `Fingerprint` and the other one
stores the second halves correspondingly. It makes the implementation easier and
also faster (checked with benchmarks) because of the assumption that it should
be almost always enough to compare only the first part and it makes key
comparison faster.

After all described optimizations were done our structure took the following form:

```haskell
data TypeRepMap (f :: k -> *) = TypeRepMap
    { fingerprintAs :: Unboxed.Vector Word64
    , fingerprintBs :: Unboxed.Vector Word64
    , anys          :: Boxed.Vector Any
    }
```

And the `lookup` function was implemented like this:

```haskell
lookup :: forall a f . Typeable a => TypeRepVector f -> Maybe (f a)
lookup tVect =
        fromAny . (anys tVect V.!)
    <$> binarySearch (typeRepFingerprint $ typeRep $ Proxy @a)
                     (fingerprintAs tVect)
                     (fingerprintBs tVect)
```

It uses a manually implemented version of the binary search algorithm optimized
for unboxed vectors. The algorithm initially performs a binary search using the
`fingerprintAs` vector only. And then, after finding the first half, walks
through the `fingerprintBs` vector.

At first, a simple naive binary search was implemented but later it was
rewritten into a cache-optimized binary search ([see the description
here](http://bannalia.blogspot.ru/2015/06/cache-friendly-binary-search.html
)) which boosted the performance significantly.

## Array-based implementation

But that’s not all. Later we noticed that every vector has the following definition:

```haskell
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)
```


As you can see it contains two `Int` fields. So we can make our representation
more optimal by using
[`Array`](https://hackage.haskell.org/package/primitive-0.6.4.0/docs/Data-Primitive-Array.html#t:Array)
instead of boxed vector and
[`PrimArray`](https://hackage.haskell.org/package/primitive-0.6.4.0/docs/Data-Primitive-PrimArray.html#t:PrimArray)
instead of unboxed vector directly in the `TypeRepMap` data type.

After all optimizations the final shape of the `TypeRepMap` is following:

```haskell
data TypeRepMap (f :: k -> Type) = TypeRepMap
    { fingerprintAs :: {-# UNPACK #-} !(PrimArray Word64)
    , fingerprintBs :: {-# UNPACK #-} !(PrimArray Word64)
    , anys          :: {-# UNPACK #-} !(Array Any)
    }
```

## Benchmarking

Initially, I was frustrated about this part because I had no idea how to create
the `Map` of 1000 elements as that means I needed to somehow generate 1000
types. But there was actually a pretty elegant solution for this puzzle —
polymorphic recursion.

Let's introduce the following data types as type-level natural numbers:

```haskell
data Z
data S a
```

Using these data types we can now implement the function which builds
`TypeRepMap` of the desired size.

```haskell
buildBigMap
    :: forall a . Typeable a
    => Int
    -> Proxy a
    -> TypeRepMap Proxy
    -> TypeRepMap Proxy
```

so when I run `buildBigMap` with size `n` and `Proxy a`, it calls itself
recursively with `n - 1` and `Proxy (S a)` at each step, so the types are
growing on each step.

But this wasn’t the only challenge in benchmarking `TypeRepMap`. There were also
a few interesting things with benchmarks to keep in mind:

* We should force maps to normal form before benchmarking.
* We can't use
  [`rnf`](https://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#v:rnf)
  function. Deriving
  [`NFData`](https://hackage.haskell.org/package/deepseq/docs/Control-DeepSeq.html#t:NFData)
  instance for `TypeRepMap` is not possible because there can be no `NFData` for
  `Any`. We won't be able to use `rnf` because it would try to force both the
  keys and the values, as our values are `Any` (can't force them), but since
  evaluating the values is not important at all for the benchmark, we could try
  to define a function like `rnf` but without touching the values.

For `Map`-based implementation we need to benchmark the `lookup` function on
different depths of our tree (as `Map` is internally a tree). But the key can be
very close to the root so our benchmarks won’t be honest enough. Thus we need to
test on different `Proxy`s with different types.

Here is the diagram of how the tree’s been constructed. You can notice that the
`Char` element is the direct child of the root:

```java
size: 16
tree:
   +--Proxy * (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))
   |
+--Char
|  |
|  |  +--Proxy * (S (S (S (S (S (S Z))))))
|  |  |
|  +--Proxy * (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))
|     |
|     +--|
|
Proxy * (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))
|
|     +--Proxy * (S (S (S (S (S (S (S (S (S (S Z))))))))))
|     |
|  +--Proxy * (S (S (S (S (S (S (S (S (S Z)))))))))
|  |  |
|  |  +--Proxy * (S (S (S (S (S (S (S (S Z))))))))
|  |
+--Proxy * (S (S (S (S (S (S (S Z)))))))
   |
   |     +--Proxy * (S Z)
   |     |
   |  +--Proxy * (S (S (S Z)))
   |  |  |
   |  |  +--Proxy * (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))
   |  |
   +--Proxy * (S (S (S (S Z))))
      |
      |  +--Proxy * (S (S Z))
      |  |
      +--Proxy * (S (S (S (S (S Z)))))
         |
         +--Proxy * Z
```

Since we can't predict how `Ord` on `TypeRep` will behave we need to select a
`Proxy` from our range randomly, however, because our types were huge we
introduced the following type family to solve that issue:

```haskell=
type family BigProxy (n :: Nat) :: * where
    BigProxy 0 = Z
    BigProxy n = S (BigProxy (n - 1))
```

While running this version of benchmarks it turned out that `rnf` function was
taking a lot of time mostly on normalisation of the enormous `TypeRep` keys
which consisted of tall nested types like `S (S (S ...))`.

So, eventually, I end up using the ghc plugin
[ghc-typelits-knownnat](https://hackage.haskell.org/package/ghc-typelits-knownnat)
and the type of the `buildBigMap` became:


```haskell
buildBigMap :: forall (a :: Nat) . KnownNat a
            => Int
            -> Proxy a
            -> TypeRepMap (Proxy :: Nat -> *)
            -> TypeRepMap (Proxy :: Nat -> *)
```

In order to benchmark `lookup` function we implemented a special function
`fromList` to use in place of the bunch of inserts, so we will be able to see
the real time measures of `lookup` operation itself.

```haskell=
data TF f where
    TF :: Typeable a => f a -> TF f

fromList :: [TF f] -> TypeRepMap f
```

Now the `buildBigMap` function will have type

```haskell
buildBigMap :: forall (a :: Nat) . KnownNat a
            => Int
            -> Proxy a
            -> [TF (Proxy :: Nat -> *)]
            -> [TF (Proxy :: Nat -> *)]
```

Benchmarks make 10 lookups to collect average performance statistics:

```haskell
tenLookups :: TypeRepMap (Proxy :: Nat -> *)
           -> ( Proxy 10, Proxy 20, Proxy 30, Proxy 40
              , Proxy 50, Proxy 60, Proxy 70, Proxy 80
              )
tenLookups tmap = (lp, lp, lp, lp, lp, lp, lp, lp)
  where
    lp :: forall (a :: Nat) . Typeable a => Proxy a
    lp = fromJust $ lookup tmap
```

and compare the work of map-based implementation with optimal array-based implementation.
Here are the achieved results:

> **NOTE:** time in the report is for 10 lookups. To get the average time of single `lookup` you need to divide time by 10.

* Benches GHC-8.4.3

```haskell
benchmarking map-based/lookup
time                 2.198 μs   (2.195 μs .. 2.202 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.196 μs   (2.193 μs .. 2.199 μs)
std dev              10.46 ns   (8.436 ns .. 12.67 ns)

benchmarking dependent map/lookup
time                 819.0 ns   (810.7 ns .. 829.1 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 815.8 ns   (812.1 ns .. 822.5 ns)
std dev              16.11 ns   (9.371 ns .. 23.09 ns)

benchmarking vector-binary-search/lookup
time                 370.7 ns   (368.9 ns .. 372.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 368.9 ns   (368.2 ns .. 369.7 ns)
std dev              2.512 ns   (1.938 ns .. 3.474 ns)

benchmarking array-cache-optimized-binary-search/lookup
time                 183.5 ns   (183.2 ns .. 183.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 183.6 ns   (183.3 ns .. 184.4 ns)
std dev              1.535 ns   (958.3 ps .. 2.631 ns)
```
## Conclusion

In this blog post, I wanted to show the difficulties, tricks, and useful
information which I personally learned during the implementation of an optimized
version of `TypeRepMap`. Also, I needed to somehow structure the knowledge I’ve
gained while working on this project. You can say that some parts of the post
can be skipped or might be irrelevant but I wrote it in such a way on purpose to
highlight the topics that I find very hard to find and understand quickly. So I
hope you too will find this knowledge useful!

## Acknowledgments

Many thanks to Vladislav Zavialov ([\@int-index](https://github.com/int-index))
for mentoring this project! It was the great experience for me.

## Bonus

A few more challenges on the way to the release `typerep-map`:

### KindOf

During interface enhancement I ran into some weird issue described below.

It’s nice to have the `member` function and it makes sense to implement it using
already written lookup function:

```haskell
member :: forall a f . Typeable a => TypeRepMap f -> Bool
member trMap = case lookup @a trMap of
    Nothing -> False
    Just _  -> True
```
Type of the `lookup` function is the following:

```haskell
lookup :: forall a f . Typeable a => TypeRepMap f -> Maybe (f a)
```

Unfortunately, this implementation of `member` doesn’t compile! The problem is
in the fact that the compiler can’t infer that type variable `a` and the
argument to `f` have the same kind. These two functions have the following type
with implicitly inferred kinds:

```haskell
lookup :: forall {k} (a :: k) (f :: k -> *) . Typeable a
       => TypeRepMap f -> Maybe (f a)
member :: forall {k1} {k2} (a :: k1) (f :: k2 -> *) . Typeable a
       => TypeRepMap f -> Bool
```

After [this ghc proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0026-explicit-specificity.rst)
is implemented, it should be possible to write such type signatures directly in
code. The current workaround is to use this trick with `KindOf` type:

```haskell
type KindOf (a :: k) = k

member :: forall a (f :: KindOf a -> Type) . Typeable a => TypeRepMap f -> Bool
```

###  New TypeRep performance

During benchmarking the `Map`-based implementation of `TypeRepMap`, very
perceptible performance degradation was noticed. Here is the comparison table
with the results we have with our `Map`-based implementation.

|   ghc version  | containers         | performance |
|----------------|--------------------|-------------|
|     8.0.2      |      0.5.7.1       |   556.9 ns  |
|     8.2.2      |      0.5.10.2      |   2.076 μs  |
|     8.4.3      |      0.5.11.0      |   2.464 μs  |



We didn’t observe this performance degradation when we used `Fingerprint` as
keys, so it's probably an issue with the new `TypeRep`.

### KnownNat and Typeable

Initial version of `buildBigMap` function had this type signature:

```haskell
buildBigMap :: forall (a :: Nat) . (Typeable a, KnownNat a) => ...
```

But, unfortunately, it became broken on GHC-8.4.3! Turned out that `Typeable`
and `KnownNat` constraints don’t play well together. This observation resulted
in the following ghc ticket with quite an interesting discussion:

* [ghc/ticket/15322](https://ghc.haskell.org/trac/ghc/ticket/15322)
