---
title: State monad comes to help sequential pattern matching
author: Dmitrii Kovanikov
tags: haskell, state, transformers
description: "Combining Monad transformers and applicative functors for better error messages report"
---

Let’s try to solve one specific problem using the `State` monad and monad
transformers to see how pure stateful computations work in Haskell and how they
can be used to write better interfaces. This blog post shouldn’t be considered
as a tutorial on the `State` monad or monad transformers, but if you’re already
familiar with those concepts, you may find the following use case interesting.

The blog post also contains several exercises, so you can follow the code by
yourself and try to expand the presented solution along with solving exercises.

## Problem

Consider the following data type:

```haskell
data Value
    = BoolValue Bool
    | IntValue Int
```

This is a plain Haskell sum type that represents possible options of parsing the
string into a more structured type.

> **NOTE:** how to parse a string into a `Value` is out of the scope of this
> blog post.

Let’s assume that you received a list of such values from some external data
source and you want to convert this list to your custom data type. More
precisely, you want to implement the following function:

```haskell
decodeValues :: [Value] -> Either VeryGoodErrorMessage MyType
```

The need for such a function may appear in various situations. The list of
`Value`s can represent many things:

1. A row in some CSV file.
2. A row in some SQL query result.
3. An array in some JSON object.

Basically, any form of unstructured data that you want to decode to structured
types.

I’m going to describe in detail, a possible general solution for implementing
`decodeValues` with an explanatory error message.

## Naive solution

Suppose that you have this Haskell data type:

```haskell
data User = User
    { userAge         :: Int
    , userIsHaskeller :: Bool
    }
```

It’s quite easy to implement a naive decoder from `[Value]` to `User` with a
poor error message:

```haskell
decodeValues :: [Value] -> Maybe User
decodeValues [IntValue age, BoolValue hask] = Just $ User age hask
decodeValues _ = Nothing
```

This function is elementary and only uses basic pattern matching to convert a
list of unstructured data to a custom Haskell data type. Sadly, the error
message is completely non-informative. Ideally, `Maybe` should only be used when
your function can fail because of a single reason. However, in our case
there are multiple possible failure scenarios for the `decodeValues` function:

1. Given list is too small.
2. Given list is too big.
3. `Value` at any index is different from what we expect.

It would be tedious to implement good error reporting using only basic pattern
matching. Moreover, the size of the function wouldn’t be small, which would and
up being a problem when you need to write variations of your function for
different types.

> **Exercise:** try to implement the `decodeValues` function for `User` that
> reports good error messages and uses only pattern matching.

The following sections of the blog post show a general solution to this problem
with fine-grained error messages.

## Parsing a single value

First of all, like any other good Haskeller, I am going to decompose this huge
problem into a few smaller ones. In the previous section, we started from
decoding whole list of values, but let’s now learn how to decode a single value.
Before that, however, we need to think about the data type of the error message.
To have a clear understanding of what’s wrong with a single value, we need to
know what the received value was and what we expected it to be:

```haskell
data SingleValueError = SingleValueError
    { singleValueErrorExpected :: Text
    , singleValueErrorActual   :: Value
    }
```

With that, we can implement functions for decoding a single `Value`:

```haskell
valToBool :: Value -> Either SingleValueError Bool
valToBool (BoolValue b) = Right b
valToBool v = Left $ SingleValueError “Bool” v

valToInt :: Value -> Either SingleValueError Int
valToInt (IntValue i) = Right i
valToInt v = Left $ SingleValueError “Int” v
```

> **NOTE:** we can remove some code duplication already at this stage! If you
> have a function like `typeName :: Typeable a => Text`
> (see [the implementation here](http://hackage.haskell.org/package/relude-0.4.0/docs/Relude-Extra-Type.html#v:typeName))
> and you derive prisms for the `Value` data type, you can implement a single
> generic `fromValue` function. But for the sake of simplicity, we won’t mind a
> bit of boilerplate code.

Once we’ve learned how to decode a single value, let's proceed with expanding
this approach for a list of values.

## Parsing a list of values: State

Just like in the previous section, let’s start with creating the data type for
our error messages:

```haskell
data ListValueError
    = UnexpectedEndOfList
    | ExpectedEndOfList (NonEmpty Value)
    | WrongValue SingleValueError
```

Let’s look closer at the constructors of this error messages data type:

1. `UnexpectedEndOfList`: this error means that the list was too short.
2. `ExpectedEndOfList`: this error means that we were given a bigger list than
   we expected. This error contains the remaining `Value`s. Note that here we
   use a `NonEmpty` list to store the remaining items because if the list was
   bigger than we expected, then it’s guaranteed to have a non-empty remainder.
   This also prevents us from writing the wrong implementation of the function.
3. `WrongValue`: this constructor stores the error for parsing a single value.

The idea of using the [`State` monad](https://hackage.haskell.org/package/transformers-0.5.5.0/docs/Control-Monad-Trans-State-Strict.html)
comes to mind after noticing the following: we can store the list of values in
the state and in our stateful action we can decode the current head of the list
and put the remaining list back to the state. Just as in basic `State` monad
tutorials where a stack data structure is used as an example.

So, initially we should create the data type for our state:

```haskell
newtype Values = Values { unValues :: [Value] }
```

That was easy. Now we need to specialize the `State` monad with our type and
give it some meaningful name. We can use type aliases but let’s be good
Haskellers and create the `newtype` as a wrapper around the `State` monad:

```haskell
newtype Decoder a = Decoder
    { runDecoder :: State Values a
    } deriving (Functor, Applicative, Monad, MonadState Values)
```

Before writing the code itself, it makes sense to describe the logic behind the
single value stateful decoder upfront:

1. We need to take a list of values from our state.
2. If the list is empty, we need to throw an error reporting that the list is
   empty and stop decoding.
3. If the list is not empty, we need to apply the given single value decoder to
   the head of the list and pattern match on the result of the decoder.
4. If the decoder results in error, we need to rethrow that error.
5. If the decoder is successful, we need to return our value and put the
   remaining list to the state.
6. Check the remaining list and if it’s not empty then throw the
   `ExpectedEndOfList` error.

After designing our decoding algorithm, we can try to implement it:

```haskell
value
    :: (Value -> Either SingleValueError a)
    -> Decoder (Either ListValueError a)
value valDecoder = gets unValues >>= \case
    [] -> pure $ Left UnexpectedEndOfList
    val:vals -> case valDecoder val of
        Left err -> pure $ Left WrongValue err
        Right a  -> Right a <$ put (Values vals)
```

> **Exercise:** in our decoder for the field we don’t report the position of the
> `WrongField` error. However, it’s quite easy to patch the decoder to take this
> into consideration.

We’re using `Either ListValueError` as the return type of our `value` decoder to
report errors. But because of that, it’s not easy to compose different `value`
decoders. This is how `Decoder` for `User` might look like:

```haskell
user :: Decoder (Either ListValueError User)
user = value valToInt >>= \case
    Left err -> pure $ Left err
    Right age -> value valToBool >>= \case
        Left err -> pure $ Left err
        Right hask -> pure $ Right $ User age hask
```

In the last section, we are going to solve this particular problem: better
composability of the value decoders.

## Parsing a list of values: StateT + Either

You may notice that the pattern matching we performed over `Either` looks
familiar. Indeed, it is the `Monad` instance for `Either`. We couldn’t use it,
because we were already using the `State` monad. This is when monad transformers
come in to save the day! In simple words, with monad transformers we can combine
monadic effects of multiple monads so the `>>=` operator performs actions for
every monad in our monad transformer state.

We want to combine `State` and `Either` effects, but in this particular case, we
should be cautious because the `State` monad transformer and the `Either` monad
transformer are not commutative. Which means that `StateT s Either` is not the same as
`EitherT (State s)`. Let’s look at this closely:

```haskell
StateT s m a ~ s -> m (a, s)
EitherT e m a ~ m (Either e a)

1. StateT  s (Either e) a ~                        s -> Either e (a, s)
2. EitherT e (State  s) a ~ State s (Either e a) ~ s -> (Either e a, s)
```

Today, let’s pick the first option. The reason for this is to stop decoding as
soon as we face an error. Only after we’ve managed to decode the raw values to
our type successfully, we can inspect the remaining list of values in case we
need to produce the `ExpectedEndOfList` error message.

Let’s refactor our existing approach to this new version, and start with
patching the type of the `Decoder` monad:

```haskell
newtype Decoder a = Decoder
    { runDecoder :: StateT Values (Either ListValueError) a
    } deriving ( Functor, Applicative, Monad
               , MonadState Values, MonadError ListValueError)
```

Then we can rewrite our `value` decoder to use `throwError` function to report
errors.

```haskell
value
    :: (Value -> Either SingleValueError a)
    -> Decoder a
value valDecoder = gets unValues >>= \case
    [] -> throwError UnexpectedEndOfList
    val:vals -> case valDecoder val of
        Left err -> throwError $ WrongValue err
        Right a  -> a <$ put (Values vals)
```

Note how the code became much cleaner! Now it’s quite easy to implement the decoder
for the `User` data type:

```haskell
user :: Decoder User
user = User <$> value valToInt <*> value valToBool
```

And it’s also straightforward to decode the list of values with the given
`Decoder`:

```haskell
decodeValues :: [Value] -> Decoder a -> Either ListValueError a
decodeValues values decoder = do  -- do-notation for the Either monad
    (a, finalState) <- runStateT (runDecoder decoder) (Values values)
    case unValues finalState of
        []       -> pure a
        val:vals -> Left $ ExpectedEndOfList (val :| vals)
```

And it works like a charm:

```haskell
ghci> decodeValues [] user
Left UnexpectedEndOfList

ghci> decodeValues [IntValue 42, BoolValue True] user
Right (User {userAge = 42, userIsHaskeller = True})

ghci> decodeValues [BoolValue True, IntValue 42]  user
Left (WrongValue (SingleValueError
    {valueErrorExpected = "Int", valueErrorActual = BoolValue True}))

ghci> decodeValues [IntValue 42, BoolValue True, BoolValue False] user
Left (ExpectedEndOfList (BoolValue False :| []))
```

> **NOTE:** In this blog post I’ve used a function-based approach for decoders.
> However, it’s also possible to use a typeclasses-based approach to avoid passing
> explicitly value conversion function for value decoder and `Decoder` action to
> `decodeValues` function.

That’s all folks! I hope that after this blog post you have gained a better insight
into the `State` monad and monad transformers.

Here you can find gist with the full code:

* [Pattern matching + State monad + Monad transformers](https://gist.github.com/ChShersh/a608386180330c6b1eb07144f0a06815)
