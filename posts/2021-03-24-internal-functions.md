---
title: "Many faces of Internal Functions"
author: Veronika Romashkina
tags: haskell, fp, guide
description: "The post explores various ways to define internal functions and discuss the advantages and disadvantages of each approach"
useShortName: yes
---


Higher-Order Function (HOF) is a fundamental concept of _Functional Programming_
(FP). HOF is a widespread and broadly used feature as it allows manipulating
functions quite easily. HOF helps us to create more abstract interfaces and
write more composable code, eventually leading to well-structured systems that
work together really well.

In Haskell, there are different ways to define and use functions. And working
with HOF often requires creating some "internal" functions for passing them as
arguments to other functions. Today I want to explore various ways to do that
and discuss the advantages and disadvantages of each approach. Knowing the pros
and cons helps you understand different syntax and semantic techniques and
establish some ground rules when working with HOF.

## HOF

A function that takes another function as an argument or returns a function is
called __Higher-Order Function__. HOF is essential to Functional Programming, as
functions are a central piece of FP. Being able to use functions as *first-class
citizens* is a crucial part of the Functional Programming paradigm. Also, it is
almost impossible to use declarative programming efficiently without first-class
functions.

Here is how HOF may look like in the type signatures:

```haskell
apply :: (a -> b) -> a -> b
multiplyBy :: Int -> (Double -> Double)
```

![Higher-Order Function](/images/posts/hof/HOF.png)

## Example of HOF

Haskell has a lot of commonly-known HOFs that are present in many other
functional languages as well. You have probably already seen or used the `map`
function – a go-to function to alter the list structures.

```haskell
map :: (a -> b) -> [a] -> [b]
```

It takes a list and a function, that tells how to transform each element of the
list, and then applies a given function to each list element.

To give you one example, if you want to get the list of `String`s out of the
list of numbers (to print them, for instance), you will write something like
this:

```haskell
showList :: [Int] -> [String]
showList xs = map show xs
```

You see that we used a standard function `show` as an argument for the `map`ping
function.

The standard library contains many more HOFs such as `filter`, foldings,
traversals, function composition, application and binding operators. Once you
start using HOFs, it's impossible to imagine programming without them.

## Non-standard HOFs

However, the already existing standard functions are not always enough for all
use-cases (otherwise, developers' roles won't be that necessary). Very likely
that your case may be more sophisticated and include a few more things that need
to be done with your data. Let's examine what options we have to handle those
cases.

## Lambda

Lambda function (anonymous function) is the first thing that comes to mind.
Lambdas provide a quick way to build a new function instantly on-the-fly.

Returning to our example of mapping numbers, let's say that instead of simply
showing the number, you want to add something to every string as well. In that
case, you can't just reuse `show`. You need to apply a custom function to each
element. Fortunately, you can define a lambda function and describe the new
desired behaviour in its body:

```haskell
showList xs = map (\x -> show x <> " is in the list") xs
```

The lambda approach is appeared to be extremely handy in the case when the
function is the last argument, so you could write multiline lambda body, without
the necessity of the old-fashioned brackets, but with the new-school `$`:

```haskell
main = do
    let xs = [0..10]
    for xs $ \x -> do
        let y = x ^ 2
        putStrLn $ "The square of " <> show x <> " is " <> show y
```

## Where

Although lambda functions are perfect for quick solutions like above, they can
quickly become overloaded and hardly readable in some other situations,
especially when they are becoming long. For increased readability and
maintainability, you can define a local function with the help of the Haskell
`where` construction.

Our `showList` example could be rewritten with this approach:

```haskell
showList = map showOne xs
  where
    showOne :: Show a => a -> String
    showOne x = show x <> " is in the list"
```

And as you see, we again pass a simple function as the `map`'s argument after
this rewrite.

This approach's handy benefit is that you can have a normally-written local
function that is visible inside the one it is defined in. You can specify the
type signature, including polymorphic variables, so it could be reused
throughout the initial function while in scope. At the same time, you won't
spoil the global namespace of functions. You also could be as generous with
commenting, and explanation as you wish while writing code in any format.

The nice example of where `where` is usually a better idea than a lambda
functions is the following:

```haskell
groupBy
    :: forall f k a
    .  (Foldable f, Ord k)
    => (a -> k)
    -> f a
    -> Map k (NonEmpty a)
groupBy f = foldl' mapGroup mempty
  where
    mapGroup :: Map k (NonEmpty a) -> a -> Map k (NonEmpty a)
    mapGroup m a = alter (Just . toVal) (f a) m
      where
          toVal :: Maybe (NonEmpty a) -> NonEmpty a
          toVal Nothing   = a :| []
          toVal (Just xs) = a <| xs
```

## Let

An alternative to `where` construction is `let-in` (or `let`). `let` is always
written before the start of some *expression* (yes, `let` could be introduced
for any internal expression as well) where it would be used. It is also
important to keep in mind that the defined `let` variable would be visible only
and only in this exact expression.

Our example then, rewritten in terms of the `let` syntax, has the following shape:

```haskell
showList =
    let showOne x = show x <> " is in the list"
    in map showOne xs
```

Usually, the type signatures are omitted in the `let` constructions by
developers; however, it is also legal (and maybe even friendlier to future
readers) to add the type signature the same way we do with `where`:

```haskell
showList =
    let showOne :: Int -> String
        showOne x = show x <> " is in the list"
    in map showOne xs
```

And here is the example of the `let` construction with the smaller scope:

```haskell
if even x
then let newVal = x `div` 2 in newVal ^ 2
else let (newVal, p) = divMod x 2 in newVal + p
```

`let` expression is frequently used with `do`-notation.

```haskell
main = do
    let name = "Veronika"
    putStrLn $ "Hello, " <> name <> "!"
```

The fundamental difference between `where` and `let` (besides the readability
and other subjective metrics) becomes pictorial when dealing with different
internal scoping layers. These two structures behave differently there. You
can't put a definition via `let` after the actual usage of it. But at the same
time, you can't use locally defined through `let` variables and functions in the
`where` clause to the initial function.

```haskell
-- WRONG!
main = do
    let name = "Veronika"
    putStrLn greet
  where
    greet :: String
    greet = "Hello, " <> name <> "!"
```

The `let` variables become beneficial when you want to reuse a binded variable
inside the `do`-block:

```haskell
knownUsers allUsers = do
    cachedUsers <- getCachedUsers
    let isKnownUser :: User -> Bool
        isKnownUser user = user `Set.elem` cachedUsers
    pure $ filter isKnownUser allUsers
```

> Fun fact: you can use `where` inside `let` and `let` inside where too. But be
> careful with the understanding of their scopes. It could easily become tricky!

## Top-level

By using either lambdas, `where` or `let`, you restrict your function from being
used elsewhere outside the function you define it in. However, you could end up
writing a very general and useful helper function that actually could be reused
in a few places throughout the file.

In this case, you may consider moving this function to the top level of the
module.

```haskell
showOne :: Show a => a -> String
showOne x = show x <> " is in the list"


...

showDoubleList :: [Double] -> [String]
showDoubleList xs = map showOne xs

showBoolList :: [Bool] -> [String]
showBoolList xs = map showOne xs
```

It then gives you a few additional benefits that you may use: you can provide
the docs with various examples, you can export it from the file and use it
elsewhere, you can make the compiler optimise it more aggressively, you can test
it separately (if you export it).

## Import

As the add-on to the previous approach, this one goes even further. The helper
function could be so useful that you need it in many parts of your project, not
only in the current module. To have a more intuitive structure and prevent
possible cyclic dependencies between modules, you can put it into a separate
module – either a new one, or to the existing one that by semantics corresponds
to the meaning of the function.

In our `showList` example, we can put the helper function into the dedicated
internal module for show-related utilities:

```haskell
import My.Show.Internal (showOne)


myList = map showOne xs
```

## Comparison

As we have now seen different approaches, let's write down explicit pros and
cons to each of them.

#### Lambda

__*Pros:*__

 * Quick solution
 * Immediately visible actions
 * No need to waste your creativity talent on naming
 * Shorter code

__*Cons:*__

 * Awkward to specify type signatures
 * Not suitable for big function bodies (especially if not the last argument)
 * Could lead to `{ ..;}` syntax, which is not widely used or easily readable
 * The scope is directly limited to this particular usage and can't be reused
 * No way to give more context through the name of the function
 * Awkward to add comments to the arguments or other parts of the function
 * You can introduce partial pattern-matching accidentally and not notice it
   because GHC doesn't warn on non-exhaustive pattern matching by default or
   even with -Wall
 * Non-discoverable in the project

#### Where

__*Pros:*__

 * Dedicated one place to look up for the helper functions of the concrete
   function
 * Doesn't litter the initial function body with the details that are not
   essential at the high-level review
 * Won't distract from the main code
 * Type signatures and comments could be added without bringing the additional
   burden
 * The scoping is clear (the whole initial function)
 * Implies the high-to-low approach of reading code (Starting from high-level
   context and going to low-level implementation details only if needed)
 * Doesn't spoil the global namespace of functions so that local names could be
   generic and short
 * Could be introduced separately for each pattern matching branch of the
   function

__*Cons:*__

 * The variables and functions created inside the body of the initial function
   (including the binding results while in do-notation) are not visible in
   `where`
 * `where` is not always exist in other languages, so people could be less used
   to it (e.g. Elm has only `let` but not `where`)
 * Some people prefer the low-to-high approach of reading code (Starting from
   low-level view context going to high-level only after knowing all low-level
   details)
 * Can't be used outside the function and won't be visible in the project
 * Polymorphic type signatures in such functions can require additional
   extensions and `forall`ing in the main function

#### Let

__*Pros:*__

 * Could be introduced right before the usage
 * Possible to specify type signatures
 * The scoping starts from the declaration of `let` till the end of the
   underneath expression
 * Could be introduced in any expression in the function

__*Cons:*__

 * The declaration is in the middle of the function, so for example, in
   functions with long bodies, it could be tricky to find where the new function
   comes from, as `let` declaration could be anywhere
 * Usually, type signatures are omitted by developers and many of them advocate
   for this practice (while we in Kowainik have different
   [style guide](https://kowainik.github.io/posts/2019-02-06-style-guide) rules
   on this topic); it makes it hard to grep on the function
 * Can't be used outside the function and won't be visible in the project
 * Polymorphic type signatures in such functions can require additional
   extensions and `forall`ing in the main function
 * Beginners are confused by `let` and binding with `<-` in do-notation
 * For some reason, it's easier to introduce non-terminating recursive
   expressions with `let`, and this causes
   [hot proposals about changing the behaviour](https://github.com/ghc-proposals/ghc-proposals/pull/401)
 * Enabling some extensions (e.g. `TypeFamilies`) implies the `MonoLocalBinds`
   extension that makes inferred types of `let` expressions less polymorphic,
   which can lead to unexpected compilation errors

#### Top-level

__*Pros:*__

 * A proper function declaration with type signature and docs
 * Easily discoverable in the project
 * Can be reused in many functions
 * Could be more polymorphic in order to fit more use-cases
 * Can be tested with doctests

__*Cons:*__

 * Can be more polymorphic than necessary, which could lead to explicit type
   annotations on usage
 * Still should be exported in order to test in the test-suits
 * Can't use variables from the local context, so requires passing everything
   explicitly to it, which may lead to lots of boilerplate
 * Need to be more careful with the name as it could lead to name-clashes

#### Import

__*Pros:*__

 * A proper function declaration with type signature and docs
 * Easily discoverable in the project
 * Have a dedicated logical place in the project and is considered to be used
   throughout the project
 * The usage in different modules is apparent, and can restrictions of usages
   can be customised through tools
 * Different optimisation rules of GHC for exported functions
 * You can test this function
 * Can be imported qualified in order to be more specific with the context
 * Can be added to the local `Prelude` module and used without any imports
   anywhere in the project

__*Cons:*__

 * Can be more polymorphic than necessary, which could lead to explicit type
   annotations on usage.
 * Can't use variables from the local context, so requires passing everything
   explicitly to it, which may lead to lots of boilerplate
 * Can perform slower than the non-imported version due to GHC optimisations
 * Need to be more careful with the name as it could lead to name-clashes
 * Requires to come up with a place for this function: it could either be
   exported from irrelevant module, or the all-purpose general `.Util`, or
   require creating a single-function module, or probably even require resolving
   cyclic module dependencies (either via boot files or by any other method)


## Conclusion

The functions that take functions as arguments are a powerful tool in writing
maintainable, readable and elegant code. And we are in charge of deciding on the
best way to make that happen in each particular case. Each solution has its own
trade-offs that affect maintainability, readability and even performance. The
above exploration is only for guidance. I am sure that each specific case and
project requires special consideration and deciding on the approach that would
work better for the project and developers.
