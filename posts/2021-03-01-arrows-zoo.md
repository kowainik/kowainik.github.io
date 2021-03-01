---
title: "Arrows Zoo"
author: Veronika Romashkina
tags: haskell, fp, guide
description: "The digest of all arrows (->, <-, =>, etc.) use cases in Haskell."
useShortName: yes
---

::: {.cite-quote}
:::: {.cite-quote-content}
A good archer is not known by his arrows but his aim
::::
:::: {.cite-quote-author}
Thomas Fuller
::::
:::


Don't know about you, folks, but I had a hard time remembering all the Haskell
arrows that you can bump into in all different situations. For example, I guess
I will never be able to use `ViewPatterns` correctly on the first attempt. For
me, the digest of every use case of each arrow in Haskell sounds like a handy
thing to have, at least this information will be structured somewhere, so here
we go.

Each section of this post covers a specific arrow application, explains when and
how it is used and gives examples. Some not that popular usages of the arrow
syntax are mentioned in this writing too. So, if you are excited, let's get
started!

![Arrows Blue Knight](/images/posts/arrows/arrows-blue-knight.jpg)

## ->

The most frequently used arrow in Haskell – right-directed arrow `->` – is the
one we are going to talk about first and foremost.

### Types

`->` is used in the type signatures to show the relation of types in functions.
When just starting learning Haskell, we can think of this arrow as an indicator
in the type. But later, we can figure out, that in reality, `->` is the built-in
data type specified in the `GHC.Prim` module:


```haskell
infixr -1 ->
data (->) a b
```

The arrow type operator is usually used in the infix form. It shows the
direction from input to output type of functions.

For instance, we say that the standard `length` function takes a list and
returns a single integer – the list's size. With the arrow, it is written in the
type like this:

```haskell
length :: [a] -> Int
```

We can use function type when defining a new data type:

```haskell
newtype ReaderT r m a = ReaderT
    { runReaderT :: r -> m a
    }
```

Another example of where you can use `->` as a type operator is
[GADT](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/gadt.html):

```haskell
data Expression where
    Literal :: String -> Expression
    Const :: Int -> Expression
    BinOperation :: BinOp -> Expression -> Expression
    ...

```

### Kinds

Like type signatures for functions and data types, we also use `->` to specify
the kinds of the types (aka types of the types).

With the
[KindSignatures](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/kind_signatures.html#extension-KindSignatures)
extensions, we can specify the kind for the type variables:

```haskell
{-# LANGUAGE KindSignatures #-}

data IntBox (f :: Type -> Type) = IntBox (f Int)
```

With the relatively new GHC extension –
[StandaloneKindSignatures](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/poly_kinds.html#extension-StandaloneKindSignatures),
which was added in GHC-8.10, we can specify the __kind__ of the type similar to
the way we do it for functions' types:

```haskell
type Pair :: Type -> Type
data Pair a = Pair a a
```

Right-directed arrow is also used in the
[type families](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html)
declarations:

```haskell
type family AllHave (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllHave _ '[]       = ()
    AllHave f (x ': xs) = (f x, AllHave f xs)
```

### Lambda function

The syntax for lambda functions in Haskell uses the right arrow `->` but
on the term-level and not type-level as in the previous use-case. The general
scheme is the following:

```
\variables with spaces (or patterns in parenthesis) -> action with variables
```

Here are a few examples:

```haskell
doubleList :: [Int] -> [Int]
doubleList = map (\x -> x * 2)

foldl' (\hash el -> HashSet.insert el hash) mempty

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

heads :: [NonEmpty a] -> [a]
heads = map (\(x :| _) -> x)
```

### Cases

__Case of__

In addition to pattern matching in the function declaration, you can also use
the __case-of__ expression. The main difference between the case-of and
top-level pattern matching is that `case` uses arrows `->` instead of `=` for
branch results.

```haskell
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust ma f = case ma of
    Just x  -> f x
    Nothing -> pure ()
```

__Lambda case__

Similar to the *case-of* expression,
[__lambda case__](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/lambda_case.html#extension-LambdaCase)
is used to reduce syntax noise when using the case construction on the last
argument in lambda functions.

The most common case is to use it on the last input argument in the functions:

```haskell
isEmpty :: [a] -> Bool
isEmpty = \case
    [] -> True
    _  -> False
```

__Case guards__

Just like *guards* in normal functions, you can use guards with cases as well in
the same manner, but keeping the syntax of the *case-of* with `->`:

```haskell
maybeAddEven :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven ma mb = case (ma, mb) of
    (Just a, Just b)
        | even a && even b -> Just a + b
        | otherwise -> Nothing
    (_, _) -> Nothing
```

__Lambda Case* __

Additionally, since GHC 9.0.1, combining `LambdaCase` with `Arrows` allows the
`\case` syntax to be used as a command in the `proc` notation:

```haskell
proc x -> (f -< x) `catchA` \case
    p1 -> cmd1
    ...
    pN -> cmdN
```

### View Patterns

[`-XViewPatterns`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/view_patterns.html?highlight=viewpatterns#extension-ViewPatterns)
is a handy extension that allows writing function application directly in the
arguments specification. And, as you can guess, the `->` is part of its syntax
too. First, you need to specify the function you want to apply and then, after
the arrow, the variable that you can use later on in the function body. E.g.

```haskell
mkUser :: Text -> Text -> User
mkUser (Text.toLower -> nickname) (Text.toLower -> name) = User
    { userNickname = nickname
    , userName = name
    }
```

You can also use `ViewPatterns` with the concrete pattern, not the variable:

```haskell
startWithA :: String -> Bool
startWithA (map toLower -> 'a':_) = True
startWithA _ = False
```

Multiple `ViewPatterns` can be composed in a single pattern:

```haskell
maybeEven :: Maybe Int -> String
maybeEven (fromMaybe 0 -> even -> isEven) =
    if isEven
    then "Even number"
    else "Other"
```

> The above is equivalent to the usage of ordinary function composition and a single view pattern:
> ```haskell
> maybeEven (even . fromMaybe 0 -> isEven)
> ```

```haskell
> maybeEven (Just 2)
"Even number"
> maybeEven (Just 15)
"Other"
> maybeEven Nothing
"Other"
```

### Multi-Way If

The
[`MultiWayIf`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/multiway_if.html#extension-MultiWayIf)
extension allows writing `if-then-else` constructions with *guards* to check
more conditions on the same level, and it also uses the right-directed arrow
(->). Check this one out:

```haskell
choose :: [String] -> IO ()
choose allowedStrs = do
    input <- getLine
    if | trim input == "" ->
           putStrLn "Empty input" >> choose allowedStrs
       | map toLower input `elem` allowedStrs ->
           putStrLn $ "You choose: " <> input
       | otherwise ->
           putStrLn "Choose wisely" >> choose allowedStrs
```

### Linear types*

The brand new feature introduced in Haskell is [__linear
types__](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html).
It only appeared in GHC-9.0 and is not yet that widespread, but it is nice to
know that the arrow with the special syntax can represent linear types. The syntax
is using `%m ->` to specify the "linearity" of the function.

See the example of the `uncons` function, that consumes the input list only once
(hence `%1`):

```haskell
uncons :: [a] %1 -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)
```

## <-

Now, let's see where the opposite-direction arrow `<-` is used. It is also quite
popular and has different use-cases, which is good to distinguish in order to
understand and control your code better.

### Do

One of the main syntactic Haskell features is __"do" notation__. Do-notation is
an alternative for building up monadic computations, using a pseudo-imperative
code writing style with the named variables. In reality, do-notation is the
syntax-sugar for the binding that uses the left arrow `<-` operator to assign
the result of binding into the variable.

There are a few types of do-notations. We are going to check out each of them
separately here.

__Traditional Do-notation__

Any instance of the `Monad` class can be used in a traditional do-block in
Haskell without any additional extension whatsoever.

The most conventional usage of do-notation is with the `IO` Monad:

```haskell
main :: IO ()
main = do
    input <- getLine
    putStrLn $ input <> " is what you wrote."
```

This is the same as:

```haskell
main = getLine >>= \input -> putStrLin (input <> " is what you wrote.")
```

> Note how our new arrow `<-` translates to the already familiar lambda-arrow
> `->` and the bind (`>>=`) operator.

Do-notation could be written with any Monad: `Maybe`, `Either`, `Reader`, `State`, etc.

> Note: do not confuse `<-` binding and `let =` expressions in the do-notation.
> `let = ` is used for simple variable assignment, when `<-` is used to assign
> the "binded" variable.
> ```haskell
> main :: ()
> main = do
>     input <- getLine
>     let reversedInput = reverse input
>     putStrLn $ "Reverse of what you wrote is " <> reversedInput
> ```

__Applicative-do__

[ApplicativeDo](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/applicative_do.html)
is a GHC extension that allows using do-notation with the `Applicative`
representers. Unlike monadic `do`, this one uses do-notation to syntax sugar
`<$>` and `<*>` operators of the `Applicative` typeclass.

Applicative-do is very useful for working with Applicative-based solutions, for
example, with [optparse-applicative](@hk) – Haskell parsing library:

```haskell
{-# LANGUAGE ApplicativeDo #-}

userParser :: Parser User
userParser = do
    userName <- nameParser
    userAge  <- ageParser
    pure (User userName userAge)
```

The above becomes the syntax sugar for:

```haskell
userParser :: Parser User
userParser = User <$> nameParser <*> ageParser
```

__Recursive-do__

`RecursiveDo` is yet another extension to strengthen do-notation. It allows
recursive bindings that won't work with the ordinary do-notation.

```haskell
{-# LANGUAGE RecursiveDo #-}

data Node = Node Int (IORef Node)

mkNode ::  IO (IORef Node)
mkNode = do
    rec nodeRef <- newIORef (Node 0 nodeRef)
    return nodeRef
```

Alternatively to the `rec` keyword for the particular binding, `mdo` could be
used instead of `do` to use recursive bindings on the whole block where GHC will
cleverly apply it optimally.

__Qualified-Do__

The
[QualifiedDo](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/qualified_do.html?highlight=qualifieddo#extension-QualifiedDo)
extension is the most recent addition to the compiler that became available only
with the GHC version 9.0.

The feature allows you to customise the do-notation sugaring rules (you can use
the `>>=` function defined in some modules in the qualified `do` blocks). You
would then be able to specify qualified for each particular case you want to use
it with.

For example, if we have a `List` module that would define binding functions:

```haskell
module List where

import Prelude hiding (Monad (..))

(>>=) :: [a] -> (a -> [b]) -> [b]
(>>=) = flip concatMap

(>>) :: a -> [a] -> [a]
(>>) = (:)
```

Then you can use it for the list do-notation:

```haskell
{-# LANGUAGE QualifiedDo #-}

import qualified List

list :: [String]
list = List.do
    num <- [10, 42]
    "Hello"
    "number"
    show num
    "!"
    []
```

And it works like this:

```haskell
> list
["Hello","number","10","!","Hello","number","42","!"]
```

You can also combine different `do`s:

```haskell
import qualified Your.Module.With.Defined.Bind as M

f :: M.M SomeType
f = M.do
    x <- foo  -- <- represents M.>>=
    foo' $ do
        y <- g1  -- <- represents Prelude.>>=
        g2 x y
```

### Comprehension

Comprehensions are an expressive shorthand for building computations on some
particular structures. There are several different comprehensions you can try on
your favourite types.

Comprehensions use the `<-` arrow to express generators that would be used for
the structure building.

__List comprehension__

List comprehension is the syntactic sugar for working on lists. See the
following examples:

```haskell
> [ (i, j) | i <- [0..2], j <- [0..2] ]
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

> [ (i, j) | i <- [0..2], j <- [0..2], i /= j ]
[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]

```

This function is actually the Cartesian product of two sets – a list of pairs of
numbers from sets of numbers from 0 to 2. Here, with `<-`, you hand over each
element of the list as an input to generate pairs.

List comprehensions are the most convenient for writing some `map`s and
`filter`s over lists.

Note that you can use functions in list comprehensions and also nested list
comprehensions as well.

__Parallel List comprehension__

One way to boost your list comprehension skills is to use the
[ParallelListComp](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/parallel_list_comprehensions.html)
extension. If we think of `concatMap`s (or cartesian products) in list
comprehensions, here, we should imagine the work of such comprehensions as
`zip*` functions.

Syntactically, `ParallelListComp` uses several `|` statements with `<-` arrows.

Let's look at the similar to the list comprehensions example to understand the
difference with better:

```haskell
> :set -XParallelListComp
> [ (i, j) | i <- [0..2] | j <- [0..3] ]
[(0,0),(1,1),(2,2)]
```

As you can see, it behaves exactly as `zip` in here, while list comprehension
would give you product.

__Transform List Comp__

The
[`TransformListComp`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/generalised_list_comprehensions.html#extension-TransformListComp)
extension enables the SQL-like syntax in list comprehensions. By SQL-like, I
mean that several keywords from the SQL world are introduced that you can
utilise in comprehension syntax: `group`, `by`, and `using`. Also, you need to
use the `then` keyword for each statement.

```haskell
> nums = [ ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5) ]
> [ (str, num) | (str, num) <- nums, then sortWith by str ]
[("five",5),("four",4),("one",1),("three",3),("two",2)]
```

__Monad comprehension__

[`MonadComprehensions`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/monad_comprehensions.html#extension-MonadComprehensions)
is the extension that allows list comprehension syntax with `Monad`s. This also
makes parallel comprehensions (Parallel List Comprehensions) and transform
comprehensions (Generalised (SQL-like) List Comprehensions) to work for any
monad.

Here is an example of how this feature can be used to write the `maybeAddEven`
function from the [Case guard](#cases) section:

```haskell
maybeAddEven :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven  ma mb = [ a + b | a <- ma, b <- mb, even a, even b ]
```

### Pattern guard

[Pattern guard](https://wiki.haskell.org/Pattern_guard) is a feature that, in
some way, extends the *guards* notion. Instead of being a boolean expression
like in simple guards, a pattern guard is a list of qualifiers, similar to the
previous section's list comprehension.

You can often use it instead of the awkward `case-of` statements.

And again, let's reuse the `maybeAddEven` example, but this time with the help
of the pattern guards feature:

```haskell
maybeAddEven :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven ma mb
    | Just a <- ma
    , even a
    , Just b <- mb
    , even b
        = Just $ a + b
    | otherwise = Nothing
```

### Pattern-synonyms

[`PatternSynonyms`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pattern_synonyms.html#extension-PatternSynonyms)
is an extension that allows you to define names for the patterns. To define some
of the patterns, you need to use `<-`. For example:

```haskell
pattern Head x <- x:xs

foo :: [a] -> Maybe a
foo [] = Nothing
foo (Head x) = Just x
```

See more about patterns [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms).

## Other Arrows

There are much more arrows for all tastes in Haskell. Here are a few more diversely
looking arrows, used in various situations, for you to enjoy.

### =>

This fat arrow is used to specify constraints in the type signatures:

```haskell
sort :: Ord a => [a] -> [a]
```

### <<= and =>>

The double-tip arrows represent operator forms of the Comonadic `extend` and
[flipped `extend`](https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html#v:-60--60--61-)
functions.

As `Comonad` is the dual to the `Monad` concept, you can notice why the
operators look this way. It is just the opposite of the monadic bind – `>>=` and
`=<<` operators.

### <<</>>>

These arrows are from the `Category` typeclass. `>>>` is the
left-to-right composition, and `<<<` – right-to-left composition (similar to the
dot (.) composition operator) in the `Category` and `Arrow`s world. The
following three examples, demonstrating different order of composition,
produce the same result:

```haskell
lowerName :: User -> Text
lowerName = toLower . userName
-- or
lowerName = toLower <<< userName
-- or
lowerName = userName >>> toLower
```

### -<

Similar to the do-notation we saw before, GHC has a special `do` syntax for
`Arrow`s. Instead of the `<-` arrow, you can use `-<` when working with the
`Arrow` typeclass:

```haskell
foo = proc x -> do
    fx <- f -< x
    gx <- g -< x
    returnA -< (fx + gx)
```

## Conclusion

Arrows in Haskell can have ambiguous meaning depending on the context. Hope,
this post could make the use-cases clearer and help understand some different
arrow usage situations.
