---
title: "Foo to Bar: Naming Conventions in Haskell"
author: Veronika Romashkina
tags: haskell, documentation, guide
description: A comprehensive guide for various naming conventions for types, variables and functions in Haskell
useShortName: yes
---

Developers spend most of their time reading code, understanding it and
exploring other ways to use existing solutions. Frankly, in our
profession, there is very little time on actually writing new
libraries and creating new interfaces in real-life development. So it
is quite important to have some help in the most common
activities. __Naming conventions__ is one such thing that improves
readability and eases the usage cost if agreed upon and spread
worldwide.

Some languages have their own special naming conventions that make
sense. Haskell is among them. There are a bunch of naming patterns
that are commonly used everywhere in the ecosystem (including the
standard libraries) that may help you to recognise the function's
meaning without looking at its documentation and even its type! This
ability is especially relevant because naming is one of the hardest
development problems, so having some help and no-brainer rules to
guide in this area improves everyone's life.

In this post, we will explore common naming conventions in Haskell
together. It is going to be useful for both creators (library and API
developers) and consumers (library users), as it establishes norms
accepted in the libraries' APIs.

> 🦋 If you are interested in other conventions and best practices on
> how to write Haskell code, you can take a look at our
> [style guide](https://kowainik.github.io/posts/2019-02-06-style-guide).

## Checked

Let's start with the conventional and straightforward norms
established in Haskell’s specifications and standards. Names in
Haskell must satisfy the following simple rules:

* __Types and typeclasses__ must start with an *uppercase* letter
* __Functions and variables__ must start with a *lowercase* letter
* __Top-level operator functions__ must start with any allowed symbol
  except for `:`
* __Constructors as operators__ must start with `:`

These rules are in the specifications and therefore checked by the
compiler. So if you try to break the naming rules, you will get errors
during the compilation.

Additionally, functions follow the __lowerCamelCase__ style and types
follow the __UpperCamelCase__ style. This is the de facto standard of
writing code in Haskell, but using distinct styles doesn’t lead to a
compiler error. Moreover, there are some testing libraries that use
*snake_case *to discover tests automatically. However, you can
restrict that with the Haskell tooling, e.g. HLint can check this for
you.

<hr>

There are various details in the names that will give you hints on
what the function does. We will walk through them to learn to
recognise them all.

## Type variables

Let's start our excursions with the type variables that are most
commonly used in type signatures and definitions. Some variables
represent particular meaning: it could be a typeclass-related
convention or just a convenient shorter usage. But in any way, this
information is useful to know while reading and writing types:

:::: {.table .row}

::: {.col-3}
`a`, `b`, `c`, ...
:::

::: {.col-9}
Ordinary type variables with no particular meaning

```haskell
map :: (a -> b) -> [a] -> [b]
```
:::

::: {.col-3}
`f`
:::

::: {.col-9}

`Functor` or `Applicative`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```
:::

::: {.col-3}
`m`
:::

::: {.col-9}
`Monad` or `Semigroup`/`Monoid`

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(<>) :: Semigroup m => m -> m -> m
```
:::

::: {.col-3}
`e`
:::

::: {.col-9}
Types representing errors

```haskell
failures :: [Validation e a] -> [e]
```
:::

::: {.col-3}
`t`
:::

::: {.col-9}

`Foldable` or `Traversable`

```haskell
concat :: Foldable t => t [a] -> [a]
```

or types of kind `Type` (previously `*`)

```haskell
newtype Size (t :: Type) = Size Int
```
:::

::: {.col-3}
`k`
:::

::: {.col-9}

Kind

```haskell
type family (++) (xs :: [k]) (ys :: [k]) :: [k]
```
:::

::::

## Function variables

The way we name arguments and variables in functions is also not
accidental. They contain hints that make reading these variables used
in function bodies easier. Variables in functions use the following
established commonly used names:

:::: {.table .row}

::: {.col-3}
`l`
:::

::: {.col-9}
Lists variable
:::

::: {.col-3}
`(x:xs)` or `(y:ys)`
:::

::: {.col-9}
Patterns for lists where `x` means a single x and `xs` means _many `x`s_

Also used together with the previous one with as-patterns:

```haskell
foo l@(x:xs) = ...
```
:::

::: {.col-3}

`f`, `g`, `h`
:::

::: {.col-9}
Function arguments

```haskell
map f (x:xs) = f x : map f xs
```
:::

::: {.col-3}
`p`
:::

::: {.col-9}
Functions which are predicates (e.g. of type `Int -> Bool`)

```haskell
bar :: (Int -> Bool) -> Int -> Maybe Int
bar p i = ...
```
:::

::: {.col-3}
`n`, `i`, `j`, `k`
:::

::: {.col-9}
Miscellaneous counters
:::

::: {.col-3}
`len`
:::

::: {.col-9}
Container size
:::

::: {.col-3}
`go`
:::

::: {.col-9}
Helper recursive function. Read more about the
[Recursive go pattern](https://kowainik.github.io/posts/haskell-mini-patterns#recursive-go).
:::
::::

## Suffixes

Suffix in a Haskell function can contain a lot of information about
its purpose. Sometimes you would see multiple different suffixes
simultaneously that combine the characteristics of each piece, so it
is helpful to pay attention to that.

#### Apostrophe '

The `'` symbol is used in the functions, for which there is a
corresponding function without the apostrophe, e.g. `foo` and `foo'`.

The apostrophe at the end means that it's a __strict__ version of a
similar function. Both functions must have the same type, but
different implementations underneath. The only difference in their
behaviour is that the one with the `'` symbol evaluates intermediate
results more eagerly.

Example:

```haskell
foldMap  :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

As you can see, both functions have the same type. But `foldMap'` is
more efficient and helps to avoid space leaks when monoidal operation
`<>` is strict in both arguments.

### Typeclasses

There is a group of symbols that is used to indicate that the function
returns the value in some context. This suffix – an uppercase letter
or word – tells us the typeclass this context should represent.

Meet the `FAM`ily  👩‍👩‍👦‍👦

#### F

The suffix `F` suggests that it works with `Functor`s in some way.

Such functions can have an alternative without the suffix `F`.

Example from the [containers](@hk) library:

:::: {.row .no-gutters}
::: {.col-6}
```haskell
alter
    :: Ord k
    => (Maybe a -> Maybe a)
    -> k
    -> Map k a
    -> Map k a
```
:::

::: {.col-6}
```haskell
alterF
    :: (Functor f, Ord k)
    => (Maybe a -> f (Maybe a))
    -> k
    -> Map k a
    -> f (Map k a)
```
:::
::::

However, sometimes the suffix `F` has an alternative meaning. `F`
frequently used as a suffix in formatting libraries to indicate that a
function is a *formatter* or a *pretty-printer*.

Example from the [fmt](@hk) library:

```haskell
timeF :: FormatTime a => Text -> a -> BuilderSource

```

You can see how the same naming can have different meanings. What's
important is that the library establishes its naming convention
explicitly and uses it consistently.

#### A

The suffix `A` means that the function works with some general
`Applicative` type (the type that has the `Applicative` instance).

Examples:

```haskell
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

#### M

The suffix `M` is among the most common ones. It usually means that
the function works with the `Monad`s or in the monadic context.

Similarly to the previous suffixes, functions *with* `M` have their
counterparts *without* `M`:

:::: {.row .no-gutters}
::: {.col-6}
```haskell
filter

    :: (a -> Bool)
    -> [a]
    -> [a]
```
:::

::: {.col-6}
```haskell
filterM
    :: Applicative m
    => (a -> m Bool)
    -> [a]
    -> m [a]
```
:::
::::

> Note: Historically, the standard Haskell library `base` didn't have
> `Applicative` functors, and there weren't the superclass of
> Monads. But now the suffix `M` is also sometimes used with
> Applicative functions.

### Underscore _

Underscore, as a suffix of functions, also has a special meaning. It
gives us a clue that the function works exactly as the one without `_`
but discards the result (returns `()` instead).

Examples:


:::: {.row .no-gutters}
::: {.col-6}
```haskell
concurrently
    :: IO a
    -> IO b
    -> IO (a, b)
```
:::

::: {.col-6}
```haskell
concurrently_
    :: IO a
    -> IO b
    -> IO ()
```
:::
::::

### Number

You can often see the series of functions with the numbers at the end
of their names. These groups have the same initial part but differ
with the number. Numbers there represent the __number of arguments__
each function takes.

> 🔢 The number 1 is not used usually in that meaning as it's redundant.

:::: {.row .no-gutters}
::: {.col-4}
```haskell
liftA  :: Applicative f
    => (a -> b)
    --  ^ 1
    -> f a -> f b
    --  ^ 1
```
:::

::: {.col-4}
```haskell
liftA2 :: Applicative f
    => (a -> b -> c)
    --  ^ 1  ^ 2
    -> f a -> f b -> f c
    --  ^ 1    ^ 2
```
:::

::: {.col-4}
```haskell
liftA3 :: Applicative f
    => (a -> b -> c -> d)
    --  ^ 1  ^ 2  ^ 3
    -> f a -> f b -> f c -> f d
    --  ^ 1    ^ 2    ^ 3
```
:::
::::

> 👀 Have you noticed the suffix `A` here? :)

:::: {.row .no-gutters}
::: {.col-4}
```haskell
zipWith
    :: (a -> b -> c)
    -> [a] -> [b] -> [c]
```
:::

::: {.col-4}
```haskell
zipWith3
    :: (a -> b -> c -> d)
    -> [a] -> [b] -> [c] -> [d]
```
:::

::: {.col-4}
```haskell
zipWith4
    :: (a -> b -> c -> d -> e)
    -> [a] -> [b] -> [c] -> [d] -> [e]
```
:::
::::

Number 1 has a special meaning of requiring at least one argument to
be present in a container:

```haskell
foldr  :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
```

Though, the fact that it works with `Foldable` is not ideal. There is
[a proposal](https://gitlab.haskell.org/ghc/ghc/-/issues/13573) to
implement a typeclass called `Foldable` (or `Semifoldable`) for
non-empty types.

### L/R

The suffixes `L` and `R` (sometimes `l` and `r`) represent the
direction of function application or order of traversing a data
structure.

> ℹ️ Most of the time, these sibling functions have the same type, but
> in some functions, certain arguments are reversed for convenience.

Examples:

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanl :: (b -> a -> b) -> b -> [a] -> [b]

mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
```

### By/On

Some of the overloaded functions that work with `Foldable`s or lists
have a non-overloaded sibling with the suffix `By`.

It is often convenient to use these functions together with `on`, for instance:

```haskell
sortBy (compare `on` fst)
```

This `on` pattern is so widely used, so functions also have the suffix `On`.

Compare these three types:


:::: {.row .no-gutters}
::: {.col-4}
```haskell
sort
    :: Ord a

    => [a]
    -> [a]
```
:::

::: {.col-4}
```haskell
sortBy

    :: (a -> a -> Ordering)
    -> [a]
    -> [a]
```
:::

::: {.col-4}
```haskell
sortOn
    :: Ord b
    => (a -> b)
    -> [a]
    -> [a]
```
:::
::::

### P

In some libraries or applications' code, the suffix `P` shows that the
function is a parser of some type, e.g. when using the
[optparse-applicative](@hk) library. The usage of this naming
convention can look like this:

```haskell
data Config = Config
    { configPort :: Port
    , configPath :: FilePath
    }

portP :: Parser Port
pathP :: Parser FilePath

configP :: Parser Config
configP = do
    configPort <- portP
    configPath <- pathP
    pure Config{..}
```

## Prefixes

Now we are going to focus on function prefixes and their
meaning. Similar to suffixes, there are some established patterns that
are often used by developers.

#### newtypes

Newtypes in Haskell is a widespread pattern. It is a wrapper around
some type. Thus, it is important to mention this relation to the type
or the fact that it's a newtype in the name.

#### un/get/run

Newtypes can have a name for their only field. One of the most common
naming conventions is to name this field as the type name prefixed
with `un` (short for *unwrap*):

```haskell
newtype Size = Size
    { unSize :: Int
    }
```

::: {.exercise}

When `un` is followed by small letter, it usually means the inverse of
the same function (short for *undo*):

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```
:::

In the standard library `base`, you can find a lot of `Monoid`al
newtypes that use the prefix `get` for the same purposes:

```haskell
newtype Max a = Max
    { getMax :: a
    }
```

However, if the newtype is some wrapper for a Monad, the prefix `run`
is utilised instead:

```haskell
newtype StateT s m a = StateT
    { runState :: s -> m (a, s)
    }
```

#### records

Fields in record data types have several well-known naming conventions
widely used in the Haskell ecosystem and probably often equally.

One popular naming rule is to prefix each field with the __full type
name__ to avoid name conflicts with other records:

```haskell
data User = User
    { userId   :: Int
    , userName :: Text
    }
```

Sometimes, the __abbreviation__ is used as a prefix when the full name
of the type is too long:

```haskell
data HealthReading = HealthReading
    { hrDate        :: UTCTime
    , hrMeasurement :: Double
    }
```

#### pretty

The prefix `pretty` is used for _pure_ functions that display values
in a prettier human-readable way, unlike `show`, which is supposed to
be parsed by Haskell.

```haskell
data GhcVersion
    = Ghc884
    | Ghc8102
    deriving stock (Show)

prettyGhcVersion :: GhcVersion -> Text
prettyGhcVersion = \case
    Ghc884  -> "GHC 8.8.4"
    Ghc8102 -> "GHC 8.10.2"
```

#### when

The `when*` family of functions usually do some actions when the
criterion is met.
Usually, the first argument is the criterion followed by the action
that needs to be run. Such functions typically discard the result of
either and return `pure ()`.

This convention is originated from the `when` function in base:

```haskell
when :: Applicative f => Bool -> f () -> f ()

-- variations
whenM        :: Monad m       => m Bool      -> m ()        -> m ()
whenJust     :: Applicative m => Maybe a     -> (a -> m ()) -> m ()
whenNothingM :: Monad m       => m (Maybe a) -> m a         -> m a
whenLeft_    :: Applicative f => Either l r  -> (l -> f ()) -> f ()
```

> 🔗 See how multiple naming conventions are used together?

Similarly, there's the prefix `unless` that has the inverse meaning
for the check: `when (not p) ≡ unless p`.

#### is

Prefix `is` is used for predicates that check some property and return
`Bool`. The property could also be a check on the constructor for sum
types or some more specific check:

```haskell
isNothing :: Maybe a -> Bool
isLeft :: Either a b -> Bool
isEven :: Int -> Bool
```

#### m

We have already seen the suffix `M`. However, `m` is also often used
as a prefix. When you see `m` in this position, it could have two
different meanings described below.

When followed by a __lowercase__ letter, it usually means that the
function works with some monadic type (similar to the suffix meaning).

```haskell
filter  ::                (a -> Bool) -> [a] -> [a]
mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a

zip  ::               [a] -> [b] -> [(a, b)]
mzip :: MonadZip m => m a -> m b -> m (a, b)
```

But when followed by an __uppercase__ letter, it usually means that
this is a `Maybe` version of a value. This naming convention is
generally used with local variables.

```haskell
printPath :: Maybe FilePath -> IO ()
printPath mPath = case mPath of
    Nothing -> putStrLn "No path given"
    Just path -> putStrLn $ "Path is: " ++ path
```

#### generic

The standard library uses the
[prefix `generic`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#g:26)
to provide functions that return polymorphic values or work with more
polymorphic arguments. They are usually much slower, as a consequence,
but in some cases, they are the best option.

:::: {.row .no-gutters}
::: {.col-6}
```haskell
take

    :: Int
    -> [a]
    -> [a]
```
:::

::: {.col-6}
```haskell
genericTake
    :: Integral i
    => i
    -> [a]
    -> [a]
```
:::

::::

#### mk

Smart constructors are usually named with the prefix `mk`, followed by the type name:

```haskell
newtype Positive = Positive Int

mkPositive :: Int -> Maybe Positive
```

Sometimes, ordinary constructors also start with the prefix `Mk`:

```haskell
newtype Size = MkSize Int
```

## Operator conventions

Haskell allows defining custom operators, and as regular functions,
they also have a few of their own naming conventions.

<hr>

Arrows around some already existing operator usually mean that it is a
*lifted* version of it in some sense:


:::: {.row .no-gutters}
::: {.col-6}
```haskell
($)

    :: (a -> b)
    -> a
    -> b
```
:::

::: {.col-6}
```haskell
(<$>)
    :: Functor f
    => (a -> b)
    -> f a
    -> f b
```
:::
::::

The number of `<>` layers can mean the number of applications of the
same concept.

:::: {.row .no-gutters}
::: {.col-6}
```haskell
(<$>)
    :: Functor f
    => (a -> b)
    -> f a
    -> f b
```
:::

::: {.col-6}
```haskell
(<<$>>)
    :: (Functor f, Functor g)
    => (a -> b)
    -> f (g a)
    -> f (g b)
```
:::
::::

In the same spirit, arrows can mean the direction of function
application:

```haskell
(<$) :: Functor f => a -> f b -> f a
($>) :: Functor f => f a -> b -> f b
```

<hr>

Some operators have `!` in them, which means they are stricter
versions of their analogues:

```haskell
($)  :: (a -> b) -> a -> b
($!) :: (a -> b) -> a -> b

(<$>)  :: Functor f => (a -> b) -> f a -> f b
(<$!>) :: Monad   m => (a -> b) -> m a -> m b
```

## Others

Haskell also introduces several additional naming conventions.

A function that handles each constructor by returning a value or
applying some action to its argument is called an __eliminator__. It
has the same name as the type and starts with a lower letter.


Examples:

:::: {.row .no-gutters}

::: {.col-6}
__Data type__
:::

::: {.col-6}
__Eliminator__
:::

::: {.col-6}
```haskell
data Bool
    = False
    | True
 
 
```
:::

::: {.col-6}
```haskell
bool
    :: a  -- ^ Handle 'False'
    -> a  -- ^ Handle 'True'
    -> Bool
    -> a
```
:::

::: {.col-6}
```haskell
data Maybe a
    = Nothing
    | Just a
 
 
```
:::

::: {.col-6}
```haskell
maybe
    :: b  -- ^ Handle'Nothing'
    -> (a -> b)  -- ^ Handle'Just'
    -> Maybe a
    -> b
```
:::

::: {.col-6}
```haskell
data Either a b
    = Left a
    | Right b
 
 
```
:::

::: {.col-6}
```haskell
either
    :: (a -> c)  -- ^ Handle'Left'
    -> (b -> c)  -- ^ Handle 'Right'
    -> Either a b
    -> c
```
:::

::: {.col-6}
```haskell
data Validation e a
    = Failure e
    | Success a
 
 
```
:::

::: {.col-6}
```haskell
validation
    :: (e -> c)  -- ^ Handle 'Failure'
    -> (a -> c)  -- ^ Handle 'Success'
    -> Validation e a
    -> c
```
:::

::::

<hr>

When functions or constructors are unsafe, they have the prefix `unsafe` or `Unsafe`.

```haskell
unsafePerformIO :: IO a -> a
unsafeFromList :: Size -> [a] -> Bundle v a
```

<hr>

Sometimes functions also have the prefix `is` or suffix `Of`/`From`
(or both) to make them read more like natural language.

Examples:

```haskell
streamOf someList
traverseOf userFollowersL
copyFileFrom somePath
"foo" `isPrefixOf` "fooBar"
```

## Possible ecosystem improvements

We highlighted some of the most common and established naming
conventions in the Haskell ecosystem. But sometimes different Haskell
libraries or particular functions don't follow common rules, and have
inconsistent or non-obvious naming rules within the library itself.

That means that not every library uses naming conventions, which is
unfortunate. It's very confusing to get used to some rules and common
sense, and then realise that they don't work in some places and your
assumptions on how something should work are incorrect. It wastes a
bit of our time and also slows down the processes. We, as a community,
should work harder on establishing and following the __best
practices__, as this is one the most topical struggles for Haskell
developers according to the
[2020 State of Haskell survey results](https://taylor.fausak.me/2020/11/22/haskell-survey-results/#s6q7).

Here are several examples of potential areas for improvement:

* In packages with container implementations, functions to extract
  `Map` keys are called `keys` but functions to extract values are
  called `elems`, not `values` which would be logically ensuing.
* Functions for converting a dictionary to a list of key-value pairs
  in `containers` is called `assocs` and in `unordered-containers` is
  called `toList`. At the same time, `toList` is also a method of
  `Foldable` and behaves precisely as `elems` in both cases.
* Generally speaking, not having a unified interface for container
  data structures (maps, sets, sequences, etc.) causes pain from time
  to time. [containers-backpack](@github(kowainik)) is one way to solve
  this problem, though the ecosystem is not yet ready for the backpack
  feature (which is 4 years old in Haskell).
* `*sql-simple` family of libraries have functions where suffix `_`
  means "no arguments" instead of "this function discards the result".
* People use apostrophe `'` to define local variables for their
  updated variables because it is too hard to come up with a new name
  that will better reflect the meaning of the new var in the
  scope. E.g. you can often see something like `let cur = f x; cur' =
  g cur; cur'' = h cur'`. This approach makes code hard to follow and
  often confusing when variables are not close to each other, and your
  first thought is that some stricter version of a function is used.
* Haskell has a feature called
  [_typed holes_](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#typed-holes).
  This feature allows using a variable starting with an underscore in
  expressions, and it lets the compiler help you with the type of the
  specified expression. However, this conflicts with lens naming
  rules: `_1` and `_2` lenses for tuples and prisms starting with `_`.
* Names in the standard library `base` are also inconsistent in some
  aspects. There are patterns, which we also described in the post,
  but some anomalies also exist. For example, newtypes like `Max` and
  `Const` have fields named `getMax` and `getConst`, but `Identity`
  (also a newtype) has the name `runIdentity`. This inconsistency can
  be very puzzling often and requires keeping in mind different naming
  conventions for values of the similar structures.
* The `m/M` letter in various functions doesn't really explain where the monadic type should go. See for yourselves:

  ```haskell
  filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
  mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
  ```

  Both functions satisfy the naming convention we were talking about in
  a sense. However, there are no logical explanations as to why exactly
  the first one is working with lists, while the other one is working
  with general `Monad`s.

::: {.exercise}

### Challenge time!

What do you think would be the type of a function called `mfilterM`?

:::

We believe that we all can do better here by embracing standard rules
and sharing this knowledge with each other.


## Conclusion

When coming to programming, one usually doesn't know anything about
the accepted rules and best practices. It takes some time (along with
the right people and resources to learn from) to feel "at home". The
same applies when coming to a new language. Naming is one of the
essential keys of code readability, usability and
understandability. So, sharing this knowledge is as much as important.

To make a community stronger, its users more confident and working as
a team, we all need to follow some common standards in naming. We hope
that our observations in this write-up could be the first steps into
some more common norms and guidelines.
