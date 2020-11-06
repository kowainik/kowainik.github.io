---
title: "Dhall to HLint: Using Dhall to generate HLint rules"
author: Veronika Romashkina
tags: haskell, hlint, dhall, prelude
description: "Creating custom HLint rules for alternative preludes with Dhall"
updated: "October 2, 2019"
---

This short blog post covers the process of creating the `.hlint.yaml` file from
the Dhall configuration. We are using [dhall-json-1.4.0](@hackage:-1.4.0) in all following code
snippets.

## Motivation

Let's first figure out why one could need custom [HLint][hlint] rules. The
default HLint settings mostly cover `base` library. However, nowadays, many
people are using alternative preludes. Having `.hlint.yaml` file specific to the
prelude library can help to migrate to the alternative prelude and use it more
efficiently.

In Kowainik, we also have our custom prelude – [`relude`][relude]. `relude`
persues lots of goals such as minimalism, convenience, user-friendliness, etc.
We are working a lot to make it pleasant to those who are using it. `relude`
functionality has a few differences from what standard `Prelude` gives us, which
means that not all of the out-of-the-box HLint rules are suitable for us. That's
why we need our own rules to provide more information about `relude` just by
running `hlint` on your project that uses `relude` as a custom prelude. To show
how helpful it could be, I list some use cases for the custom HLint rules below.

* __Reexports__

  `relude` brings common types and functions (from packages like `containers`
  and `bytestring`) into the scope because they are used in almost every
  application. So we need a way to assist users in getting rid of the redundant
  imports since `GHC` can't warn about such imports. `hlint` lets us do it with
  a `warn` rule. For example:

  ```yaml
  - warn:
      name: Use 'ByteString' from Relude
      lhs: Data.ByteString.ByteString
      rhs: ByteString
      note: '''ByteString'' is already exported from Relude'
  ```

* __Lifted functions__

  In `relude` a lot of `IO` functions are lifted to `MonadIO` for the convenience.
  All similar HLint suggestions have the following structure:

  ```yaml
  - warn:
      name: '''liftIO'' is not needed'
      lhs: liftIO (print x)
      rhs: print x
      note: If you import 'print' from Relude, it's already lifted
  ```

* __Relude specific rules__

  We also would like to encourage people to make the code more efficient and
  clear. In `relude` we introduced some functions with the improved performance
  comparing to the `base` analogues. For instance, with the following rule, you
  can perform much faster nubbing:

  ```yaml
  - warn:
      lhs: nub
      rhs: ordNub
      note: '''nub'' is O(n^2), ''ordNub'' is O(n log n)'
  ```

* __Ignoring__

  Also, as we are constructing a renewed interface, not all `base` rules are
  suitable. Consequently, we need to ignore some of them:

  ```yaml
  # `relude` doesn't export untotal `head`.
  - ignore:
      name: Use head
  ```
* __Hints__

  Users might not want to perform all changes we suggest, so instead of `warn`s
  we can offer `hint`s on some less significant rules:

  ```yaml
  - hint:
      lhs: (fmap and (sequence s))
      rhs: andM s
      note: Applying this hint would mean that some actions that were being
            executed previously would no longer be executed.
  ```

To summarise everything said before, we have to write a lot of boilerplate to
cover all these rules which we (as lazybones) would like to avoid at all costs.
Moreover, the maintenance price is quite high for thousands of lines of `yaml`.

## Why Dhall

As the tool that can help us with removing boilerplate, we have chosen [Dhall language][dhall]. As reference:

> Dhall is a programmable configuration language that is not Turing-complete.
> You can think of Dhall as: JSON + functions + types + imports

This sounds like pretty much what we need.

You may wonder why we are not using `Haskell` for such purposes (though we love
it so much ♥). The answer is that we don't need `IO` capabilities for our
problem; totality and safety of Dhall are enough here. Changing the
configuration in Haskell requires to recompile the whole program before
generating config, but with Dhall there's no such extra step. Not to mention
fantastic string interpolation that Dhall has. Also, we wanted to familiarise
ourselves with the new technologies, and this seemed like an excellent
opportunity to dive into Dhall.

## Implementation

Basically, HLint file is just a list of different kind of rules. List in Dhall
should consist of the elements of the same type (because Dhall is a typed
configuration language), but, as you've seen, we need to use `warn`, `ignore`,
`hint` and other rules. To unify different rule types with the same type, we can
create a sum type in Dhall. Here how you do it (this is the
`hlint/Rule.dhall`][Rule] file):

```haskell
< Arguments :
    { arguments : List Text }
| Ignore :
    { ignore : {name : Text} }
| Warn :
    { warn :
        { name : Optional Text
        , lhs  : Text
        , rhs  : Text
        , note : Optional Text
        }
    }
| Hint :
    { hint :
        { lhs  : Text
        , rhs  : Text
        , note : Optional Text
        }
    }
>
```

This type might look like this in `Haskell`:

```haskell
data Rule
    = RuleArguments Arguments
    | RuleIgnore Ignore
    | RuleWarn Warn
    | RuleHint Hint

newtype Arguments = Args
    { arguments :: [Text]
    }

newtype Ignore = Ignore
    { ignore :: Name
    }

newtype Name = Name
    { name :: Text
    }

newtype Warn = Warn
    { warn :: Wrn
    }

data Wrn = Wrn
    { name :: Maybe Text
    , lhs  :: Text
    , rhs  :: Text
    , note :: Maybe Text
    }

newtype Hint = Hint
    { hint :: Hnt
    }

data Hnt =
    { lhs  :: Text
    , rhs  :: Text
    , note :: Maybe Text
    }
```

Since we've introduced the main `Rule` type, we should create functions for
adding rules. I'm going to explain it on one example for the reexport warnings.
The rest can be found in [`hlint/warn.dhall`][warn].

We need to implement a Dhall function that takes a type name for which we are
applying the rule and the module from which people can export it because they
don't know that it's already in `relude`, and we expect this function to output
the HLint warning about the redundant import.

Let's try to do this. Talking in Haskell syntax, we need a function like
`warnReexport :: Text -> Text -> Rule` (in our case, the rule is `RuleWarn`).
Let's write the similar one in Dhall.

```haskell
-- import Rule type
let Rule = ./Rule.dhall
-- get all constructors of `Rule`, so we can refer to it as `rule.Warn` etc.
let warnReexport
    : Text -> Text -> Rule
    = \(f : Text) -> \(mod : Text) ->
        -- using our constructor to create the `Warn`
        Rule.Warn
        { warn =
            { name = Some "Use '${f}' from Relude"
            , lhs = "${mod}.${f}"
            , rhs = "${f}"
            , note = Some "'${f}' is already exported from Relude"
            }
        }
in
    { warnReexport = warnReexport
    , ... -- here we have other functions
    }
```

And, the most important part is how actually this function is used. Let's
look at [`hlint/hlint.dhall`][hlint.dhall].

```haskell
-- import functions from previous file
let warn           = ./warn.dhall
-- reassign function
let warnReexport   = warn.warnReexport
in
    [ warnReexport "ByteString" "Data.ByteString"
    , warnReexport "Text" "Data.Text"
    ...
    ]
```

So with such rules, users would be getting a warning when they import
`ByteString` from `Data.ByteString` module of `base` instead of omitting it in
favour of the `relude` exports.

And in such manner, we could create functions and build all other rules,
pretty easy.

Now, the only thing left is to generate `.hlint.yaml` from `hlint.dhall`. To do
so, you can run the following command (you need to have
[`dhall-json`][dhall-json] installed):

 ```shell
 $  dhall-to-yaml --omitNull <<< './hlint/hlint.dhall' > .hlint.yaml
 ```

## Conclusion

The process of creating `Dhall` modules was not very painful, so we can consider
the experiment successful, and we are going to maintain the `.hlint.yaml` using
our `hlint.dhall` configuration. Now, adding additional rule *is just one line*,
and it's much easier to refactor configuration!

Here is the table of file size comparison in different formats to show you how
many keystrokes we managed to avoid.

|          | __.hlint.yaml__ | __hlint.dhall__ |
| -------- | --------------- | --------------- |
| Lines    | 3086            | 1019            |


[relude]: https://github.com/kowainik/relude
[hlint]: https://github.com/ndmitchell/hlint
[dhall]: https://github.com/dhall-lang/dhall-lang
[dhall-json]: https://hackage.haskell.org/package/dhall-json
[Rule]: https://github.com/kowainik/relude/blob/main/hlint/Rule.dhall
[warn]: https://github.com/kowainik/relude/blob/main/hlint/warn.dhall
[hlint.dhall]: https://github.com/kowainik/relude/blob/main/hlint/hlint.dhall
