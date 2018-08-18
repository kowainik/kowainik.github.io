---
title: "Picnic: put containers into a backpack"
author: Dmitrii Kovanikov
tags: haskell, backpack, containers, cabal, tutorial
---

This blog post walks the reader through the Backpack implementation of the
uniform interface for containers (`Map`s and `Set`s). We will go through the
reasons for choosing Backpack, a comparison with a typeclass-based approach, and
a basic tutorial for Backpack. You can find the proposed solution in the
following repository:

* [kowainik/containers-backpack](https://github.com/kowainik/containers-backpack)

## Motivation

The Haskell ecosystem contains multiple libraries that implement `Map`-like data
structures. Among them there are structures like balanced binary-search trees,
Patricia trees, [hash array-mapped trees](https://vaibhavsagar.com/blog/2018/07/29/hamts-from-scratch/), including
lazy and strict versions, and many others. Even though existing implementations
contain functions with the same name and structurally the same type, they don’t
share a common interface because such an interface doesn’t exist. Which means
that each time you want to use a type with a `Map`-like or `Set`-like interface,
you have to choose a concrete data type explicitly. This straightforward
approach has the advantage of having predictable and explicitly specified
performance and memory characteristics for each data type. However, it reduces
code reusability and composability, in particular:

1. It’s not easy to switch from one data type to another: we need to change
   package dependencies, imports, data type name and function names.
2. We often want to write functions that can work for any `Map` or `Set`
   (consider benchmarks). Or like in the following example:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Map (Map)
import qualified Data.Map as M

groupBy :: forall k f a . (Foldable f, Ord k)
        => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f = foldl' mapGroup M.empty
  where
    mapGroup :: Map k (NonEmpty a) -> a -> Map k (NonEmpty a)
    mapGroup m a =
        let val :: Maybe (NonEmpty a) -> NonEmpty a
            val Nothing   = a :| []
            val (Just xs) = a <| xs
        in M.alter (Just . val) (f a) m
```

And here is an example of using this function:

```haskell
ghci> groupBy even [1..10]
fromList [ (False,  9 :| [7,5,3,1])
         , (True,  10 :| [8,6,4,2])
         ]

ghci> groupBy (`mod` 3) [1..10]
fromList [ (0,  9 :| [6,3])
         , (1, 10 :| [7,4,1])
         , (2,  8 :| [5,2])
         ]
```

It would be great if we could reuse this `groupBy` function with any `Map`-like
data structure. Moreover, for most popular data types like `Map`, `HashMap`, or
`IntMap`, we can’t really write a more efficient version. It would be more
convenient to have all functions like `groupBy` implemented once and have the
ability to choose a specialized implementation based on the needs of the
program.

To explore what people would think of such an interface [we made a
poll](https://twitter.com/vronnie911/status/1020701995164884992) recently. And
it looks like people are apprehensive but interested about having a unified
interface. From the feedback we received in the comments, Backpack looks like
the most suitable solution for this problem at the moment. So we accepted the
challenge and decided to try using Backpack. This blog post contains the
detailed description of our results.

## Solution with typeclasses

Backpack is a relatively new technology. However, people have been making
attempts to implement a common interface for `Map`s long before that. One of the
most famous examples is the
[`mono-traversable`](http://hackage.haskell.org/package/mono-traversable-1.0.9.0/docs/Data-Containers.html)
package. We tried to create an alternative solution in
[`relude`](https://github.com/kowainik/relude). To get an idea of how a
typeclass-based solution might look like, you can take a look at this extract
from `relude`:

```haskell
class StaticMap t where
    type Key t :: Type
    type Val t :: Type

    size   :: t -> Int
    lookup :: Key t -> t -> Maybe (Val t)
    member :: Key t -> t -> Bool

class StaticMap t => DynamicMap t where
    insert     :: Key t -> Val t -> t -> t
    insertWith :: (Val t -> Val t -> Val t) -> Key t -> Val t -> t -> t
    delete     :: Key t -> t -> t
    alter      :: (Maybe (Val t) -> Maybe (Val t)) -> Key t -> t -> t
```

The nice thing about the `StaticMap` typeclass is that it allows to generalize
`Map` and `Set` with a single typeclass. `Set a` can be considered as a special
case of `Map a ()` or `Map a a` where each value is equal to the corresponding
key.

> **NOTE:** The difficulties in unifying `Set` and `Map` arise with functions like
> `insert`, where the number of arguments is different. You can read this
> [StackOverflow question regarding possible solutions](https://stackoverflow.com/questions/51462005/unified-interface-between-functions-of-different-number-of-arguments)
> to this problem. Still, it looks like there’s no completely satisfactory
> approach.

Aside from the difficulties mentioned above, there are still several issues with
the typeclass-based solutions:

1. The types of the generalized functions are less approachable, in particular
   due to the use of type families.
2. Performance is not predictable and often worse because type class
   specialization is not guaranteed.

Having a typeclass can potentially decrease performance because of the way
typeclasses are implemented in Haskell. For something like _access to the
database_ the overhead of a type class is not noticeable, but for operations
with pure maps it’s a huge disadvantage to have non-optimized functions. In
addition to not having a performance overhead, Backpack also contains a lot of
other features (read [motivation behind
Backpack](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst#motivation)
to see the whole list).

Below we look at how Backpack can be used to implement an interface for
`Map`-like data structures without the disadvantages of type classes.

## Backpack

The general idea behind using Backpack to solve this problem is simple. First,
you write the types of the desired methods — **signatures**. And then you
implement these signatures for the different data types. But there are different
challenges in using this approach for `Map`-like data types:

1. Data types are different across libraries (like `Map` and `HashMap`).
2. Every library has its own constraints for the keys (`Map` requires `Ord`
   constraint and `HashMap` requires `Eq` and `Hashable` constraints).
3. Types might have different kinds (consider `Map` and `IntMap`).
4. Some maps don’t have efficient modification operations since they are based
   on arrays (`Map` from [`primitive-containers`](http://hackage.haskell.org/package/primitive-containers) package).
5. Different libraries implement the same functions with different constraints.

But don’t worry, we are going to describe how we overcame these issues.

### Map from containers

First, let’s put all signatures into a separate package that contains only the
signature file. The Cabal file for this package looks like this:

```cabal
cabal-version:       2.0
name:                containers-sig
version:             0.0.0
build-type:          Simple

library
  hs-source-dirs:      src
  signatures:          Map
  build-depends:       base
  default-language:    Haskell2010
```

Note that there’s no `exposed-modules` field in the `containers-sig.cabal` file.
Instead, there’s a `signatures` field. Signatures are files with extension
`.hsig`. Such files contain only an abstract description of an interface
expressed through the type signatures (name **signatures** comes from the idea
that modules can have signatures just like functions can have type signatures).
Here’s is how the signature for `Map`s might look like:

```haskell
signature Map
          ( Map
          , Key
          , empty
          , alter
          ) where

data Map k v
class Key k

instance (Show k, Show v) => Show (Map k v)

empty :: Map k v
alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
```

Unlike ordinary Haskell modules, a signature starts with the `signature` keyword
instead of `module`. We declare the `Map` data type without a definition. The
type is parametrized by two type variables: `k` for keys and `v` for values.
Implementations of operations such as `lookup` or `insert` might need to perform
various operations on keys (compare them, take their hashes, etc), so we also
define a constraint on keys — `Key`. Signatures can also contain required
instances for data types. And then we can put the names of functions with their
types.

> **NOTE:** The example above contains only one possible version of the signature
> for `Map`. In our case, we went with minimal required set of functions for our
> examples.

Now we can implement a `groupBy` function in terms of a `Map` interface rather
than a particular implementation. This function can be in the same package with
the signature or in another package.

For a Haskell module there is no difference between signature files and other
Haskell modules, you can import signatures just like you would import an
ordinary Haskell module. Let us now put all functions that are implemented for
the general interface into the `containers-contrib` package. The Cabal file for
this package has the following content:

```cabal
cabal-version:       2.0
name:                containers-contrib
version:             0.0.0
build-type:          Simple

library
  hs-source-dirs:      src
  exposed-modules:     Map.Contrib.Group
  build-depends:       base
                     , containers-sig
  default-language:    Haskell2010
```

And here is the implementation of `groupBy` that uses only the `containers-sig`
package:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Map.Contrib.Group
       ( groupBy
       ) where

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Map (Key, Map)
import qualified Map as M

groupBy :: forall k f a . (Foldable f, Key k)
        => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f = foldl' mapGroup M.empty
  where
    mapGroup :: Map k (NonEmpty a) -> a -> Map k (NonEmpty a)
    mapGroup m a =
        let val :: Maybe (NonEmpty a) -> NonEmpty a
            val Nothing   = a :| []
            val (Just xs) = a <| xs
        in M.alter (Just . val) (f a) m
```

> **NOTE:** You can see that the implementation is almost the same!
>
> ```diff
> diff --git a/Map.hs b/containers-contrib/src/Map/Contrib/Group.hs
> index 7b5e4e4..a47dea6 100644
> --- a/Map.hs
> +++ b/containers-contrib/src/Map/Contrib/Group.hs
> @@ -1,4 +1,4 @@
> -groupBy :: forall k f a . (Foldable f, Ord k)
> +groupBy :: forall k f a . (Foldable f, Key k)
>          => (a -> k) -> f a -> Map k (NonEmpty a)
>  groupBy f = foldl' mapGroup M.empty
>    where
> ```
>
> It is still very simple Haskell: the type signatures are readable, no
> complicated features are used, no scary error messages, no performance
> overhead. For comparison, look at how `groupBy` is implemented for
> `DynamicMap` in `relude`:
>
> ```haskell
> groupBy :: forall f t a . (Foldable f, DynamicMap t, Val t ~ NonEmpty a, Monoid t)
>         => (a -> Key t) -> f a -> t
> groupBy f = foldl’ mapGroup mempty
>   where
>     mapGroup :: t -> a -> t
>     mapGroup m a =
>         let val :: Maybe (NonEmpty a) -> NonEmpty a
>             val Nothing   = x :| []
>             val (Just xs) = a <| xs
>         in alter (Just . val) (f x) m
> ```
>
> The implementation is the same, but types are not as straightforward.

However, from package management side there’s a huge difference between an
ordinary package and a package that depends on some signature. You can’t use the
`groupBy` function and see the result of its evaluation without a real
implementation for signatures (though you can implement another function that
works for any signature based on `groupBy`). So you might think of the
`containers-contrib` package as a kind of _function_ on the package level.
Backpack basically allows to convert usual packages into functions.

So `containers-contrib` is a function that takes a component like a library or
an executable with an implementation of `containers-sig` and returns an ordinary
package.

Now, let’s proceed to writing down the real definitions behind our signatures.
We are going to put the implementation of signatures into a separate package
called `containers-ordered-strict`. We don’t need to depend on `containers-sig`
package to do so. But we are aware of our signatures, so we are going to produce
a `containers-sig`-compatible module. To implement this signature, we need to
export all types and classes with the same names and kinds, and all functions
with the same names and types from the module with the implementation. Our
signature is very similar to `Map` from the `containers` package, so let’s
implement it first:

```haskell
{-# LANGUAGE ConstraintKinds #-}

module Map.Ord
       ( Map
       , Key
       , empty
       , alter
       ) where

import qualified Data.Map.Strict as M

type Map = M.Map
type Key = Ord

empty :: Map k v
empty = M.empty

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = M.alter
```

It’s necessary to eta-reduce type-aliases because of the way Backpack works.
There are some difficulties due to this limitation, but this will be covered
later.

There’s also one important thing in the cabal file for our
`containers-ordered-strict` package. Let’s have a closer look at it:

```cabal
cabal-version:       2.0
name:                containers-ordered-strict
version:             0.0.0
build-type:          Simple

library
  hs-source-dirs:      src

  exposed-modules:     Map.Ord
  reexported-modules:  Map.Ord as Map

  build-depends:       base, containers
  default-language:    Haskell2010
```

Having `reexported-modules` field is a vital part of our interface. Backpack
matches the name of the signature module with the name of the module with the
implementation. There’s an alternative way to achieve the same result, but it
requires creating even more packages.

Now we can finally use our example from the `containers-contrib` package. Let’s
implement the example in the `containers-example` package. First, we need to
specify in the `container-example.cabal` file that we want to use the
`containers-contrib` package with our `containers-ordered-strict` package as an
implementation of the `Map` signature. This can be done in the following way:

```cabal
cabal-version:       2.0
name:                containers-example
version:             0.0.0
build-type:          Simple

executable map-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  build-depends:       base
                     , containers-ordered-strict
                     , containers-contrib

  default-language:    Haskell2010
```

That’s all! Backpack will substitute the implementation of the `Map` signature
from the `containers-ordered-strict` package. This is because we used
`reexported-modules` field to give the `Map.Ord` module the same name as the
signature, and there are no other modules named `Map` in the dependencies.

Our final example looks like this:

```haskell
module Main where

import Map.Contrib.Group (groupBy)

main :: IO ()
main = do
    putStrLn "### Map ###"
    print $ groupBy (`mod` 2) ([1..10] :: [Int])
```

After compiling and running, it works like a charm!

```shell
$ cabal new-exec map-exe
### Map ###
fromList [(0,10 :| [8,6,4,2]),(1,9 :| [7,5,3,1])]
```

### HashMap from unordered-containers

We now have one implementation of the `containers-sig` package. But that’s not
enough. The key point is to have a generalized interface for different
implementations. It doesn’t make much sense to have an interface with only one
implementation, so let’s create our interface for `HashMap` from the
`unordered-containers` package. Similarly to the `containers-ordered-strict`
package, we are creating the `containers-unordered-strict` package.

> **NOTE:** At this point, you might notice that Backpack requires to create many
> packages. And that’s true. We could put the implementation inside
> `containers-ordered-strict` under a different module name, but in our case we
> don’t want to have extra dependencies if the user is only interested in a
> single implementation. Also, it wouldn’t work well with `reexported-modules`
> approach since we can’t reexport two different modules under same name.

The Cabal file for `containers-ordered-strict` is almost the same. And the
implementation of the signature could look like this:

```haskell
{-# LANGUAGE ConstraintKinds #-}

module Map.Hash
       ( Map
       , Key
       , empty
       , alter
       ) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M

type Map = M.HashMap
type Key a = (Eq a, Hashable a)

empty :: Map k v
empty = M.empty

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = M.alter
```

However, this doesn’t compile with the following error:

```haskell
   • Type constructor ‘Key’ has conflicting definitions in the module
      and its hsig file
      Main module: type Key a =
                     (ghc-prim-0.5.2.0:GHC.Classes.Eq a,
                      hashable-1.2.7.0:Data.Hashable.Class.Hashable a)
                     :: Constraint
      Hsig file:  class Key k
      Illegal parameterized type synonym in implementation of abstract data.
      (Try eta reducing your type synonym so that it is nullary.)
    • while checking that containers-unordered-strict-0.0.0:Map.Hash
      implements signature Map in
      containers-contrib-0.0.0[Map=containers-unordered-strict-0.0.0:Map.Hash]
```

So, does it mean that we can’t use our type alias? Not really. To overcome this
problem, we will use the following trick:

```haskell
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

class (Eq k, Hashable k) => Key k
instance (Eq k, Hashable k) => Key k
```

Since we can’t use type alias for `Key`, we need to create our own type class
with `(Eq k, Hashable k)` constraint. And implement this type class for every
type that has specified constraints.

And now if we want to use our `HashMap` instead of `Map` in our example, we only
need to replace these line in cabal file

```
    , containers-ordered-strict
```

with the following line:

```
    , containers-unordered-strict
```

That’s all! It’s extremely easy to switch implementations when using Backpack.

### Use several implementations inside a single package

Since we now have two implementations of our `Map.hsig` interface, we might want
to use them both in a single package. Unfortunately, we can’t just add
`containers-unordered-strict` package to dependencies in addition to
`containers-ordered-strict` and enjoy. Since both packages have a reexported
module with the same name `Map` we will see a compilation error about
conflicting definitions. To solve this problem, Cabal has the `mixins` field
inside stanza. Our `containers-example.cabal` file will now look like this:

```cabal
cabal-version:       2.0
name:                containers-example
version:             0.0.0
build-type:          Simple

executable map-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  build-depends:       base
                     , containers-ordered-strict
                     , containers-contrib

  mixins:              containers-contrib (Map.Contrib.Group as Map.Contrib.Group.Ord)
                                 requires (Map as Map.Ord)
                     , containers-contrib (Map.Contrib.Group as Map.Contrib.Group.Hash)
                                 requires (Map as Map.Hash)

  default-language:    Haskell2010
```

This syntax allows to specify what module should be used as an implementation
for a specific signature and under which name. The `as` part is essential
because in case of multiple implementations we do need to have modules with
different names.

And our example will now look like this:

```haskell
module Main where

import qualified Map.Contrib.Group.Hash as HM (groupBy)
import qualified Map.Contrib.Group.Ord as M (groupBy)

main :: IO ()
main = do
    putStrLn "### Map ###"
    print $ M.groupBy (`mod` 2) ([1..10] :: [Int])
    putStrLn "### HashMap ###"
    print $ HM.groupBy (`mod` 2) ([1..10] :: [Int])
```

You can see that Backpack has the ability to use multiple implementations of a
single interface. But they should be accessible under different namespaces.

### IntMap from containers

Now, this data type is more difficult. First, keys don’t have constraint like
`Ord`, they only have the specific monomorphic type `Int`. Second, `IntMap` data
type has kind `Type` while `Map` from signature has kind `Type -> Type`. But
these problems are also solvable. To fix the issue with constraint, our `Key`
constraint can be just type equality to `Int`. This can be achieved by partially
applying type equality operator `(~)` to the `Int` type. Regarding different
kinds: we can introduce our custom newtype with phantom type parameter. So the
final solution looks like this:

```haskell
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Map.Int
       ( Map
       , Key
       , empty
       , alter
       ) where

import qualified Data.IntMap.Strict as M

newtype Map k v = IM { unIM :: M.IntMap v }
    deriving newtype (Show)

type Key = (~) Int

empty :: Map k v
empty = IM M.empty

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k = IM . M.alter f k . unIM
```

This solution is not perfect. And it’s still an open question how to make it
better. Possible improvements are:

1. Figure out how to remove the `Key` constraint from functions like `toList`
   (such functions can know that `k` type variable is `Int` only when the
   constraint is in scope).
2. Use `coerce` to implement all functions and give more guarantees on
   performance.

But at least this solution doesn’t require advanced type-level features like
`-XTypeFamilies` (despite the fact that this extension is enabled — it’s needed
only for type equalities). It’s still plain Haskell. And we need to solve these
problems only inside the `Map.Int` module, consumers of the signature interface
don’t need to be aware of any complicated tricks. You can still write
polymorphic functions without worrying about how particular instances are
implemented internally.

### Map from primitive-containers

As I mentioned at the beginning of this blog post, some `Map`-like types are
optimized for lookups. Thus they don’t support efficient modification
operations. And that’s why it’s necessary to somehow separate read-only maps
from modifiable maps. When you use typeclasses, you can easily have two
different typeclasses with different names. With signatures, it’s also easy to
achieve the desired result. We only need to move our updating operations into a
separate package. To do so, we need to create a signature with exact same name
but in another package. So now we have two packages with the same signatures:

* `containers-sig-readonly`
* `containers-sig`

Signatures are externally extensible, which means that you don’t need to open an
issue to the main repository that contains the root of all signatures to extend
a signature. You can just create a `.hsig` file in your library, add couple
extra functions you require there, and you can reuse other functions from the
existing signature if you specify the package with signature in the
dependencies. That’s all. No need to specify a superclass constraint for your
custom type class. The signature merging algorithm will do this work for you. In
simple words, if you have two signatures with the same name, they can be merged
into a single signature if data types and classes have equal kinds and if the
functions with same names have the same types.

Both `containers-sig-readonly` and `containers-sig` packages should contain the
`Map.hsig` file with `data Map k v` and `class Key k`. That’s the only
boilerplate required. When both packages are used, `Map` data type and `Key`
class will be merged with the corresponding types from other signatures.

So if, let’s say, the authors of `containers-backpack` package forgot to add
some vital function to the interface, you could implement your own signature and
implementation, and still enjoy the rest of the ecosystem without any problems!

## Conclusion

As the result of this Backpack journey, the `containers-backpack` repository
contains the following packages:

* `containers-sig-readonly`: signatures for read-only maps
* `containers-sig`: signatures for maps that can be modified
* `containers-ordered-strict`: implementation of signatures for the
  [`Map.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html)
  data type from the
  [`containers`](https://hackage.haskell.org/package/containers) package
* `containers-int-strict`: implementation of signatures for the
  [`IntMap.Strict`](https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-IntMap-Strict.html)
  type from the [`containers`](https://hackage.haskell.org/package/containers)
  package
* `containers-unordered-strict`: implementation of signatures for the
  [`HashMap.Strict`](http://hackage.haskell.org/package/unordered-containers-0.2.9.0/docs/Data-HashMap-Strict.html)
  from the
  [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers)
  package
* `containers-primitive`: implementation of signatures for the
  [`Map.Lifted.Lifted`](http://hackage.haskell.org/package/primitive-containers-0.2.0/docs/Data-Map-Lifted-Lifted.html)
  from the
  [`primitive-containers`](http://hackage.haskell.org/package/primitive-containers)
  package
* `containers-contrib-readonly`: general functions for maps implemented using
  the `containers-sig-readonly` package
* `containers-contrib`: general functions for maps implemented using the
  `containers-sig-readonly` and `containers-sig` packages
* `containers-example`: package that mixes signatures and different implementations

Whoa, that’s a lot of packages! But life should become more relaxed once support
for multiple public libraries in cabal is implemented:

* [cabal/issues/4206](https://github.com/haskell/cabal/issues/4206)

Backpack is a really great and exciting way to develop Haskell libraries despite
the number of packages it requires. This approach is extremely new and
unexplored. And I encourage everyone to try it!

For now, these are the  opportunities I see with Backpack:

* Functions that work with a `String/Text/ByteString`-unified interface (the
  initial and most popular use case).
* [`unpacked-containers`](https://hackage.haskell.org/package/unpacked-containers):
  make polymorphic containers more efficient.
* Use `Int8/Int16/Int32/Int64/Word8/Word16/Word32/Word64` as keys in the
  `IntMap` data type without massive code duplication and performance overhead.
* Unified interfaces for data structures like `Map`s and `Graph`s. This should
  also reduce the amount of boilerplate required for benchmarks.
* Write code with the help of lens signatures that can later be replaced with
  either `microlens` or `lens` or something else to not have both packages in
  the dependencies.

I might be wrong in some cases, but Backpack is still a very useful tool that
you can put in the backpack of your Haskell skills.
