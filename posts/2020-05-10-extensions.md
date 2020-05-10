---
title: 'Extensions'
author: Veronika Romashkina
tags: haskell, tool, GHC, cabal
description: 'Non-boring Extensions in Haskell'
useShortName: yes
---

Recently, while working on some Haskell projects I desperately needed to get the
list of all extensions used in a particular module. Quite a common wish, if you
ask me, how hard could that be? But I was quite surprised that I can't easily
extract this information from simply processing a single file: there's no
existing tool to inspect modules in a required way, straightforward grep usage
is not sufficient, even
[`.hie` files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)
(compile-time info produced by the compiler) don't help me! So I've spent quite
some time investigating the root of the issue and, to be honest, I discovered
quite interesting information on the topic, which I decided to share in a blog
post for everyone's curiosity.

The write up provides the rough description of Extensions Technique in the
Haskell compiler, therefore, is suitable for everyone even without any prior
knowledge of that term. However, the main focus is going to be on the overall
concept and some internal details rather than the highlight of any particular
extensions. I am not the Haskell compiler guru, so all the provided information
is based on the ~~googling~~ research made, some prior knowledge and
understanding that I had. If you would like to read more about other aspects of
the topic, I linked some interesting and useful resources for further reading.

## Extension: what is that?

Haskell is an extremely powerful yet constantly developing language. And this is
one of the main advantages of it. The research in the programming languages area
is still ongoing, and Haskell users are quite lucky to get to test the newest
features without feeling that the language design is unstable or immature. This
is only feasible due to the standard mechanism used to introduce new features to
the Haskell compiler, specifically to the most popular one – Glasgow Haskell
Compiler (aka __GHC__). The mechanism is called _extensions_.

Extensions are the opt-in solution for enabling non-standard features supported
by the compiler, meaning, you need to explicitly ask the compiler to use the
feature if you want to play with it. It is completely fine if you don't request
any add-ons from the compiler. However, it is okay to use any extensions you
need, extensions are not some unsteady or experimental feature. And it is even
possible to bring __ALL__ extensions in scope, however, this doesn't sound like
a good idea. Even though the extensions should not conflict with each other by
design, some of them severely impact the way you write code. So you'd better
think more carefully about what you are bringing in.


Extensions are convenient and necessary for the full-strength development, but
if they are so important who gets to decide on what is coming in the next
compiler version? Usually, the life cycle of a potential extension looks like
this:

1. Some person (let's call her Alice) comes up with some interesting idea of
   what she wants to do with the language but with horror realises that it is
   currently not possible (or hard and hacky) to achieve with the current stack
   of instruments of the compiler. As this is something new, it could be done
   through the Extensions approach.
2. Alice then writes a document describing what changes she would like to see,
   what the new extension should do, how it interacts with the existing
   features, and why it is beneficial for the language. All that should be done
   with respect to the existing rules of
   [ghc-proposals](@github(ghc-proposals)).
3. After the long and thoughtful process
   [the GHC committee](https://github.com/ghc-proposals/ghc-proposals#who-is-the-committee)
   could give a positive or negative verdict according to a lot of factors.
4. Let's say that the proposal is approved, then it is a matter of implementers
   time availability when (in what compiler release) this new extension will
   come to life. By the way, an implementer could be any person, including the
   proposal authors but not necessarily them, so watch closely for coming
   proposals, and, if you feel like it, indicate your will to implement
   something yourself, this is completely free!

## More insights about extensions

Extensions sound fun, but you think that you don't need and don't use them,
though is this so? Let's figure out the kinds of extensions and the ways
extensions are used in code, and see if you can write code without a single
usage of them.

There are (at least) three types of extensions with their own twist, meaning and
the application area. Each of them is being processed differently as well and
have different effects on the work of your program. Here is the extensions
classification:

 * (Ordinary) Extensions
   - Enabled
   - Disabled
 * Haskell Language Standard Extensions
 * Safe Haskell Extensions

I am going to tell more about each group later on. Meanwhile, if you are curious
about how you can learn about particular extensions and what they do, I would
suggest reading the official
[GHC documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)
on that topic. In the reading shelf section at the end of this post, I provide
some more interesting blog posts on extensions in a more non-official and (some)
in a beginner-friendly format.

## Ordinary Extensions

This type of extensions is the most commonly used one. Usually, when we talk
about extensions we mean this group as it is where fresh extensions are added
and it is the progressive section of the GHC. Such gorgeous stars as
[`StarIsType`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StarIsType),
[`ScopedTypeVariables`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ScopedTypeVariables),
[`DerivingStrategies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-strategies)
and many others belong to this group.

Each extension has its twin villain brother. It always starts with the `No` word
followed by the name of the original (hero) extension. The villain superpower is
to disable the extension in the scope if it is present. However, the `No`
extension is completely harmless if the original extension is not in scope.

These extensions are represented in GHC by the
[`Extension`](https://hackage.haskell.org/package/ghc-boot-th-8.10.1/docs/GHC-LanguageExtensions-Type.html#t:Extension)
data type which is the representation of the _"enabled"_ extensions only. This
means that to express enabled VS disabled extensions you need another data type
and additional parsing functions. The
[`DynFlags` type](https://downloads.haskell.org/~ghc/8.10.1/docs/html/libraries/ghc-8.10.1/DynFlags.html#t:DynFlags)
(huge data type that stores all flags for a GHC session) contains the `OnOff
Extensions` field that could help with extracting extensions from a module,
still, GHC doesn't expose this type and it is used only internally.

### How to use

Okay, now you may have lots of questions like:

* how do I add the extensions that I absolutely love to every single module of
  my package?
* or how do I add the extensions which I love (but a bit less) in the particular
  stanza (like library or tests section) of my package?
* or even how do I add the extensions that seem, ahem, ~~scary~~ interesting to
  only one module of my package?

To cover those questions, let's move to the next part of this post where I
describe how extensions are specified for your package and how they affect your
modules compilation rules in each case.

#### Cabal 'default-extensions' per stanza

Haskell build tools allow you to specify extensions you want to enable (or
disable) for more than one module at a time, specifically for the whole stanza.
In your `.cabal` file you can set the
[`default-extensions`](https://cabal.readthedocs.io/en/latest/cabal-package.html?highlight=default-language#pkg-field-default-extensions)
or
[`other-extensions`](https://cabal.readthedocs.io/en/latest/cabal-package.html?highlight=default-language#pkg-field-other-extensions)
(don't @ me) properties to the list of extensions you want to be applied to
every module in the corresponding stanza.

```haskell
library
...
  default-extensions:   DerivingStrategies
                        TypeApplications
```

The nice thing about this approach is that both `default-extensions` and
`other-extensions` are so-called build-information fields, which by Cabal rules
means that they can be used in [common stanza](https://vrom911.github.io/blog/common-stanzas).
So you can easily add particular extensions to EVERY module of your project
without much boilerplate. See the example below:

```haskell
common common-extensions
  default-extensions:   DerivingStrategies
                        TypeApplications

library
  import: common-extensions
  ...

executable my-exe
  import: common-extensions
  ...

test-suite my-test
  import: common-extensions
  ...

```

#### Per module LANGUAGE pragmas

If you want to configure some extensions on module-level there is a way to tell
the compiler about your intention. There is a type of instructions to the
compiler you can write in order for GHC to understand you. Such instructions are
called __pragmas__. Luckily for us, there exists a special type of pragmas —
[`LANGUAGE` pragmas](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#language-pragma)
— and I am going to talk only about this kind in here as it is designed for
extensions.

There are a few rules to use these pragmas.

 * Each pragma should start with `{-#` followed by keyword `LANGUAGE` and closed
   by `#-}`.
 * Language pragma is a file-Header pragma: it should be the first thing in the
   module ⇒that means that they should go strictly before the `module` keyword,
   `import` keyword or the first function.
 * You can specify any number of pragmas in the file.
 * Comments won't affect the precedence.
 * Each pragma could include one or more comma-separated extensions.

For example, your file header can look like this:

```haskell
-- Here come extensions
{-# LANGUAGE ScopedTypeVariables , TypeApplications #-}

{- Absolutely
   necessary
-}
{-# LANGUAGE DerivingStrategies #-}

{- | Module blah blah documentation
-}

module ...
```

#### Command-line pragma invoking

If that is not sufficient for you, you can go full bad-ass mode and specify
extensions to enable/disable through the command-line options! It is called
[language options](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/flags.html#language-options)
and the syntax is straightforward. Add `-X` before the name of the extensions
and feed it to the ghc invoking command.

```haskell
{-# LANGUAGE Extension #-} ≡ -XExtension
```

Saying that it means that you can directly apply these options through the
command line arguments, or you can also use Cabal's `ghc-options` field for
that.

Options is a particularly good way to enable extensions in
[GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html).
You can apply the same rule for the extensions name as the option in GHC, and
add the command `:set` before, to actually enable/disable extension:

```haskell
ghci> :set -XDerivingStrategies
ghci> ...
ghci> :set -XNoDerivingStrategies
```

And also you can use `:unset` GHCi command to disable the extension as well

```haskell
ghci> :set -XDerivingStrategies
ghci> ...
ghci> :unset -XDerivingStrategies
```

#### Evil OPTIONS_GHC

As you see from the previous section all extensions are just the GHC options as
a matter of fact. That actually means that they can be in another type of pragma —
[OPTIONS_GHC](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#options-ghc-pragma).

The same rules of pragmas are applied to them, except the keyword is
`OPTIONS_GHC` instead of `LANGUAGE`, and you should specify GHC flags, not
extensions:

```haskell
{-# LANGUAGE DerivingStrategies #-}
-- is the same as
{-# OPTIONS_GHC -XDerivingStrategies #-}
```

HOWEVER! NOTE! CAUTION! DANGER! Do NOT do that, please. This is considered to be
an antipattern and it is not recommended to use in this way. It is suggested to
use `LANGUAGE` pragmas instead where possible (unless you want to piss everybody
off).

### One string to rule them all?

Even though the title is so tempting, there is no way currently to turn on all
the extensions at once. Even though there was once a GHC flag that could
activate a hell of the extensions —
[-fglasgow-exts](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#ghc-flag--fglasgow-exts).
But GHC developers are moving away from the idea to enable batches of things and
are encouraging more thoughtful and granular extensions usage.

## Haskell Language Standard Extensions

As the Haskell language develops rapidly, for example, a lot of extensions were
added to the compiler features, some of them are crucial for the smooth work
with Haskell as we know it. Haskell went through several milestones during
development. Such milestones lead to the Language Standard Reports, where a lot
of Haskell specific stuff was determined. At the moment of writing this post,
there are two such reports: [Haskell98](https://www.haskell.org/onlinereport)
and [Haskell2010](https://www.haskell.org/onlinereport/haskell2010).

As the report's purpose is to standardise the language, extensions could not
just pass by. There exist the corresponding standards of extensions which
outgrew into another kind of extensions — Haskell Language Standard Extensions.


> GHC documentation on extensions says that the default extensions are specified
> in the reports respectfully, however, I can not provide links to that as I
> didn't find this particular information in the report. What I found →
>
> * Haskell98 report doesn't contain information about default extensions
> * Haskell2010 has this information, but the set of extensions defined in the
>   report is different from the set of extensions in GHC
> * Reports don't mention, what extensions should be enabled if no language is
>   specified
>
>   - [Haskell98 Pragmas section](https://www.haskell.org/onlinereport/pragmas.html)
>   - [Haskell2010 Extensions section](https://www.haskell.org/onlinereport/haskell2010/haskellch12.html#x19-19100012.3)

Haskell Language Extensions include the following extensions:

 * [`Haskell98`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Haskell98)
 * [`Haskell2010`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Haskell2010)

In point of fact, this type of the extensions just provides the alias for the
bunch of ordinary extensions that would be turned on with the one of Haskell
Language Standard Extension.

### What is inside?

Let's reveal what is inside each of the Haskell Language Extension.

__Haskell98__:

 - [`ImplicitPrelude`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoImplicitPrelude)
 - [`StarIsType`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-StarIsType)
 - [`CUSKs`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-CUSKs)
 - [`MonomorphismRestriction`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoMonomorphismRestriction)
 - [`NPlusKPatterns`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NPlusKPatterns)
 - [`DatatypeContexts`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-DatatypeContexts)
 - [`TraditionalRecordSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoTraditionalRecordSyntax)
 - [`NondecreasingIndentation`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/bugs.html#extension-NondecreasingIndentation)

__Haskell2010__:

 - [`ImplicitPrelude`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoImplicitPrelude)
 - [`StarIsType`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-StarIsType)
 - [`CUSKs`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-CUSKs)
 - [`MonomorphismRestriction`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoMonomorphismRestriction)
 - [`DatatypeContexts`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-DatatypeContexts)
 - [`TraditionalRecordSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoTraditionalRecordSyntax)
 - [`EmptyDataDecls`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-EmptyDataDecls)
 - [`ForeignFunctionInterface`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoPatternGuards)
 - [`PatternGuards`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-NoPatternGuards)
 - `DoAndIfThenElse`
 - `RelaxedPolyRec`

__Default__ — if no Haskell Language is specified in any way (see the next
section) it doesn't mean that there will not be any extensions enabled for you.
Actually the following list of extensions is there exactly for such situations
(see also [this note](http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html)):

 * __Haskell2010__
   __-__ [`DatatypeContexts`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-DatatypeContexts)
   __+__ [`NondecreasingIndentation`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/bugs.html#extension-NondecreasingIndentation)

So in order to start with the blank list with zero extensions, you should
explicitly turn some stuff off first, do keep this in mind.

### Particularity

As it is already seen Haskell Language Standard Extensions are special and not
like the others in terms of what they mean, give and how they work. But as
promised it is not the only difference.

Canonically, a single Haskell Language extension should be specified for the
whole project, but this is, of course, not an obligation, rather a polite wish.
For your convenience, there are `default-language` and `other-language` (don't @
me) fields in the cabal file, where you can specify one of the Language
extensions you fancy. And same as for `default-extensions` fields, you can put
`default-language` under your common stanza.

You can skip explicit language specification, if you want to use the default
list of extensions I uncovered before, however building with cabal then will
give you the following warning, in case you don't find warnings irritating and
can live with that:

```haskell
Warning: Packages using 'cabal-version: >= 1.10' must specify the
'default-language' field for each component (e.g. Haskell98 or Haskell2010).
If a component uses different languages in different modules then list the
other ones in the 'other-languages' field.
```

### Language Extensions Usage

In addition to the dedicated `default-language` field in the `.cabal` file, you
can use any of the mentioned methods in the ["Usage" section](#how-to-use) of
ordinary extensions to specify the language. As a quick reminder, it could be:

 - Cabal `default-extensions` field
 - LANGUAGE Pragma
 - CLI GHC option
 - OPTIONS_GHC pragma


For the visual example of how it all interacts with each other, let's make a
small experiment.

In the Haskell file, let's put the following data type:

```haskell
data EmptyType
```

I choose to define a data type without constructors, as it requires the
[`EmptyDataDecls`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=typefamilies#extension-EmptyDataDecls)
extension, which is included into `Haskell2010` and the default list when no
Language is specified but is not present in `Haskell98`.

Let's try to compile our file without any options or LANGUAGE pragmas:

```shell
$ ghc LanguageExtensions.hs
[1 of 1] Compiling Main             ( LanguageExtensions.hs, LanguageExtensions.o )
Linking LanguageExtensions …
```

It works! We know that it should work as it takes up the default extensions
which contain the one that we need.

Let's now use the GHC option to specify the Language extension we want without
changing the file:

```shell
$ ghc LanguageExtensions.hs -XHaskell98
[1 of 1] Compiling Main  ( LanguageExtensions.hs, LanguageExtensions.o ) [flags changed]

LanguageExtensions.hs:1:1: error:
    • ‘EmptyData’ has no constructors (EmptyDataDecls permits this)
    • In the data declaration for ‘EmptyData’
  |
1 | data EmptyData
  | ^^^^^^^^^^^^^^
```

Just as expected. This file shouldn't work without manually enabling
`EmptyDataDecls` with the `Haskell98` language.

This one should work like a charm:

```haskell
{-# LANGUAGE Haskell98      #-}
{-# LANGUAGE EmptyDataDecls #-}

data EmptyType
```

Let's add more spice. I modified the file to look like this at the moment:

```haskell
{-# LANGUAGE Haskell2010 #-}

data EmptyType
```

And I am trying to compile it with the same options:

```shell
$ ghc LanguageExtensions.hs -XHaskell98
[1 of 1] Compiling Main             ( LanguageExtensions.hs, LanguageExtensions.o )
Linking LanguageExtensions ...
```

And it is successful, which also makes sense as the order of the Language
extensions matters, and the latest one should take over, and, in this case,
it is `Haskell2010`.

## Safe Haskell Extensions

> Though Haskell is predominantly type-safe, implementations contain a few
> loopholes through which code can bypass typing and module encapsulation. This
> paper presents Safe Haskell, a language extension that closes these loopholes.
> Safe Haskell makes it possible to confine and safely execute untrusted,
> possibly malicious code. By strictly enforcing types, Safe Haskell allows a
> variety of different policies from API sandboxing to information-flow control
> to be implemented easily as monads. Safe Haskell is aimed to be as unobtrusive
> as possible. It enforces properties that programmers tend to meet already by
> convention. We describe the design of Safe Haskell and an implementation
> (currently shipping with GHC) that infers safety for code that lies in a safe
> subset of the language. We use Safe Haskell to implement an online Haskell
> interpreter that can securely execute arbitrary untrusted code with no
> overhead. The use of Safe Haskell greatly simplifies this task and allows the
> use of a large body of existing code and tools.

Safe Haskell is the family of compiler extensions that are created to bring some
guarantees about the code.

You have probably noticed the __"Safe Haskell"__ field in the documentation
pages of your favourite packages on Hackage and was a bit worried, why the
safety field is determined as __"None"__ in the most type-safe language?

![Safe Haskell: None](https://user-images.githubusercontent.com/4276606/81501498-98a4a000-92d0-11ea-9108-24f6cc18ceb4.png)

Don't worry about that, probably it is not what you think it is. It just means
that the `SafeHaskell` extension is not determined by the maintainers for this
particular module.

Let's get deeper into this. There are three extensions coming with SafeHaskell:

 * [`Safe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#extension-Safe)
 * [`Trustworthy`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#extension-Trustworthy)
 * [`Unsafe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#extension-Unsafe)
 * `None`

### Specialty

As you can see, `SafeHaskell` extensions do have a very different semantic
meaning. Moreover, there are much more differences that make them into a
separate group of extensions. Let's talk a bit about them now.

 * There are no `No` extensions and there's no way to disable such extensions
   using some other language pragma.
 * Therefore, you can't `:unset` `SafeHaskell` extensions in GHCi as there is no
   villain twin with `No` prefix for it. So basically, no way to unspecify
   `SafeHaskell` extensions or respecify it if you already set it once.
 * These extensions are mutually exclusive and there can't be more than one type
   of `SafeHaskell` extension enabled in the module. This leads to compile
   error, which is caught at the preprocessing stage.
 * In GHC, `SafeHaskell` extensions are not part of the `Extensions` data type
   and are moved into the separate `SafeHaskellMode` type.
 * `DynFlag` stores the `safeInfer` and `safeInferred` boolean flags to judge
   about code _safety_ in terms of `SafeHaskell` for the current module and for
   modules depending on it.
 * `None` can not be specified manually. It is just the default value for the
   `SafeHaskell` if no extension is specified explicitly.
 * `Trustworthy` doesn't give any guarantees, it is more like a backdoor in
   `SafeHaskell` to tell GHC that the module is actually safe which, frankly
   speaking, is not always the case.
 * `SafeHaskell` doesn't allow usage of some particular extensions, so GHC won't
   compile modules that disregard these rules. See the `SafeHaskell`
   documentation for more concrete rules on that.

### Enabling SafeHaskell

Safe Haskell extensions can be used the same way as ordinary extensions, with
regard to its specialties. You can not disable these extensions and you can not
specify more that one per module.

```haskell
λ: :set -XSafe
λ: :set -XTrustworthy
ghc: <no location info>: Incompatible Safe Haskell flags! (Safe, Trustworthy)
Usage: For basic information, try the `--help' option.
λ: :set -XNoSafe
Some flags have not been recognized: -XNoSafe
λ: :unset -XSafe
Some flags have not been recognized: -XNoSafe
```

Besides all valid ways to specify SafeHaskell flags there are also special flags
and options that are related only to SafeHaskell. I won't provide a detailed
description, you can check the documentation for particularly interesting ones
for you:

 * [`-fno-safe-haskell`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--fno-safe-haskell)
* [`-Wsafe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--Wsafe)
* [`-Wno-safe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html?highlight=wno%20safe#ghc-flag--Wtrustworthy-safe)
* [`-Wtrustworthy-safe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--Wtrustworthy-safe)
* [`-Wunsafe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--Wunsafe)
* [`-Wno-unsafe`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--Wunsafe)
* [`-distrust ⟨pkg⟩`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--distrust%20%E2%9F%A8pkg%E2%9F%A9)
* [`-distrust-all-packages`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--distrust-all-packages)
* [`-fpackage-trust`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--fpackage-trust)
* [`-trust ⟨pkg⟩`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#ghc-flag--trust%20%E2%9F%A8pkg%E2%9F%A9)

## Extension Resolution Flow

Now when we know about all the ways where and how you can specify desired
extensions, you may wonder, how these completely different approaches interact
with each other? Let's break it down then. Here is the high-level description of
how extensions are being interpreted for each module in the project:

- Cabal gets Haskell standard (if applicable) that could be specified in the
  `default-language` or `other-languages` fields of the cabal file
- Cabal gets `default-extensions` (`other-extensions`) from the corresponding
  stanza of the `.cabal` file
- Cabal transmits all gathered extensions as CLI options to the GHC for
  compiling the file
- GHC analyses GHC flags to get `SafeHaskell`, and Haskell Language extensions
  as the special types of extensions
- GHC gets the list of extensions from the `LANGUAGE` pragma(s) of the module
- GHC parses extensions to get the list of Enabled/Disabled extensions
  preserving order
- GHC decides on the list of default extensions depending on the received
  information about Haskell Language Standard used
- GHC merges all extensions strictly by the order of appearance

The merge algorithm is simple enough:

 * Start with the list of default standard extensions
 * If you get the `Enabled` extension, add it to the final list (if not exist
   already)
 * If you get the `Disabled` extensions, then delete corresponding `Enabled`
   extension from the list (if exist); otherwise do nothing

To decide on the Haskell Language Standard extension the following algorithm
could be used:

 * If no extensions for this is specified, use nothing (GHC will use the default
   in that case)
 * If only one of `Haskell98` or `Haskell2020` is specified, then use it
 * If several extensions of such type are specified, use the latest one (in
   order of appearance)

For `SafeHaskell` extensions, it is a bit more tedious, as these extensions
conflict with each other, and may also conflict with some other enabled
extensions as well. Each Haskell module could only have 1 type of enabled
extensions. You can skip its definition in your file, but you can't say
`{-# LANGUAGE None #-}` even though `None` is a valid state of `SafeHaskell`
status.

## Stranger things

Going through the all of the extension materials and source code for each of the
components involved I discovered a few amusing facts, that I didn't know about,
or had the wrong understanding, so here are some of them for your pleasure:

* __Cabal has its own Extensions data type__ that is not aligned with the GHC's
  one. It makes sense to me now, but I expected Cabal to share the Extensions
  type with GHC. In reality, cabal potentially supports more compilers, and its
  job is just to parse all specified extensions, give them to the compiler as it
  is and leave the check to the latter.
* __CPP vs Cpp__: The correct way to spell the C-preprocessor extension is
  `CPP`. and this is how the constructor in the Cabal extensions type is called.
  However, the GHC constructor to represent this extension is called `Cpp`. For
  some reasons, casing for this particular extension is different.
* __Safe Haskell Extensions are not part of the GHC Extensions data type__, but
  are included in the Cabal Extensions type.
* __Haskell Language Extensions are neither part of the Cabal Extensions data
  type nor the GHC extensions type__, but you can put them in the
  `default-extensions` field instead of `default-language` in the `.cabal` file
  and it will be recognized properly. (I really don't know how it works)
* But if you put another __random unknown extension__, like `DependentTypes`
  into `default-extensions`, Cabal won't be able to find the plan to build your
  package:

    ```haskell
    $ cabal build lib:extensions
    Resolving dependencies...
    cabal: Could not resolve dependencies:
    [__0] next goal: extensions (user goal)
    [__0] rejecting: extensions-0.0.0.0 (conflict: requires DependentTypes)
    [__0] fail (backjumping, conflict set: extensions)
    After searching the rest of the dependency tree exhaustively, these were the
    goals I've had most trouble fulfilling: extensions
    ```

* __Rank2Types doesn't exist!__ It is just a synonym you can use to write that
  you want to turn on `RankNTypes`, and I actually never had mercy on the
  compiler (as I thought) by giving them lower requirements where I was sure
  that higher was not required (everywhere basically).
* __Generaliz(s)ed newtype deriving__ — this is also just a parsing type alias,
  and there is only one true (sorry, British English adepts) extension —
  `GeneralizedNewtypeDeriving`
* __`NamedFieldPuns`__. It is the `RecordPuns` constructor in the GHC Extensions
  type, and actually, you can use `RecordPuns` in both Cabal and GHC and it will
  work as `NamedFieldPuns` but with a deprecation warning.
* __`PolymorphicComponents`__ is the Cabal synonym of `RankNTypes` as well.

## Difficulties in having 'give me extensions per module' tool

If you are still not convinced that extensions are not that straightforward,
here is the summary of the main difficulties that one could face on their way to
answer any question about extensions in the module.

 * There are LOTS of ways to specialise the extensions for the module.
 * Some of the ways are through the CLI arguments which complicate tool work.
 * To retrieve information from CLI and other places you need to be able to work
   with GHC API which is
    a. Complicated
    b. Not 100% documented
    c. Heavy
 * Cabal has its own interface, types and functions to get the information about
   extensions. Cabal's API is also not the most straightforward.
 * To get extensions only local to that file you can't reuse any GHC parsing
   functions because information about extensions is not stored in AST.
 * Things like CPP won't let you see all the possible extensions in the module
   if you use GHC API
 * And much more small difficulties on the way for an average Haskell developer.

## Our solution – 'extensions' library

The previous paragraph was pretty depressing if you want to just get all
extensions declared in your modules without depending and making not-easy tricks
on GHC API to give it all to you. Especially sad, if you don't want to compile
the whole project to get into some internal information that GHC keeps. Yes, I
know that feeling. That is why we together with
[Dmitrii](https://kodimensional.dev/) started an experimental project for a
swift and easy way to give that information to users. Check out what we
achieved:

 * [kowainik/extensions](@github)

Answering the main question, I can happily say that the library works for the
most common cases. But let's look into it deeper.

This library provides functions and data types to work with all described kinds
of Haskell extensions.
Furthermore, we split the library API in such a way that you can get information
coming from cabal side and module-specific information separately, which allows
you to perform more flexible analysis depending on your needs.

#### Cabal part

For parsing Cabal files we are using [Cabal](@hackage), the library itself. It
has all functions to get the `Extensions` for each stanza and our task was to
extract that information carefully and structure it in the most efficient way.

One of the challenging tasks was to traverse the whole Cabal AST properly to get
all modules from all places in each stanza. But the most interesting issue was
to match the Cabal and GHC extensions type, which, as I mentioned before, was a
tricky part. The function that converts between different extension types has
more than 100 cases, and some of them are not straightforward, because some
extensions have different names, some are deprecated, some comes from another
compiler, some are supported only in the latest GHC version, and so on.

#### Module part

The module retrieval part is implemented through the custom parser. But in order
to be able to write one, we needed to understand all the possible syntax
allowances, which we did, and I tell you what, this is some complicated stuff.

![Pragmas Syntax](https://user-images.githubusercontent.com/4276606/81501432-46fc1580-92d0-11ea-952e-8b218bb49601.jpg)

Our goal is to support all most common cases, which includes `CPP` handling, as
it is quite a widely used technique. And this adds some more complexity to the
parser. Still, I think, it is a great value for a not too big cost.

And to understand some of the variety that can be in the header section of your
file (if you thought that there is nothing complicated there) just look at this
weave of comments, pragmas and `CPP`s together:

```haskell
{-# LANGUAGE
-- is this weird?
   {- is it? -} ScopedTypeVariables {- let me think -}
#if __GLASGOW_HASKELL__ < 810
  {- or is't it?.. -}
  TypeApplications,
  {- Oh boy, this is one complicated
      language pragma!
  -}
#else
  -- this is fine though
      -- I said, this is fine
           LambdaCase
#endif
-- innocent comment at the end"
#-}
{- We can also always use strategies -}
{- This -} {-# {- is -} LANGUAGE {- pragma for -} DerivingStrategies {- extension -} #-} {- ! -}

{-# OPTIONS_GHC
    -freverse-errors
  #-}
{-#                        LANGUAGE
  DerivingVia
                   ,
   {- ( ͡° ͜ʖ ͡°) -} Trustworthy
  #-}
```

### CLI

Based on that interface we also created a CLI tool, that you can install
already, to check extensions of your packages and modules. It is quite easy to
use and doesn't require any additional knowledge of Haskell Extensions
mechanism.

You should definitely give `extensions` a try, especially if you seek how to get
out of the following situations:

1. Get all extensions in all modules, combined with default-extensions from the
   .cabal file.
2. Get all extensions for a specific module, combined with Cabal extensions.
3. Get extensions defined only in a module.
4. Get extensions only from Cabal file.


All this information is provided in
[README](@github(kowainik):extensions#extensions) as well for better user
experience. You can follow the installation instructions and start playing with
`extensions` right now.

## Conclusion

This post doesn't have an aim to offend anybody, or any tool. It is just my
observations on the overall experience on the immersion into the Extension
topic. Yes, the experience is not ideal, some documentation is not obvious and
you constantly need to check internal and sources of lots of things to
understand the ideas better. But I hope that this post could help somebody to
understand where to find any info they need on the topic. Or it could help to
clarify things for more people (for example, by improving the higher level
docs). And, of course, showcases how to create a solution to a particular
problem.

That's all, folks.

## Reading shelf

 * [Hackage: `extensions`](https://hackage.haskell.org/package/extensions)
 * [GHC User Guide: Language Options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)
 * [GHC Language Extensions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/intro.html)
 * [Haskell Wiki: Language extensions](https://wiki.haskell.org/Language_extensions)
 * [GHC User Guide: Safe Hakell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html)
 * [Haskell Wiki: Safe Haskell](https://wiki.haskell.org/Safe_Haskell)
 * [TypeClasses: Introduction to GHC language extensions](https://typeclasses.com/extensions-intro)
 * [24 Days of GHC Extensions](https://ocharles.org.uk/pages/2014-12-01-24-days-of-ghc-extensions.html)
 * [A guide to GHC's Extensions](https://limperg.de/ghc-extensions/)
 * [School of Haskell: Language Standards](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/language-standards)
 * [Safe Haskell paper](https://simonmar.github.io/bib/papers/safe-haskell.pdf)
 * [Haskell High Performance Programming: Enforcing type-safety using Safe Haskell](https://books.google.co.uk/books?id=rIVcDgAAQBAJ&pg=PA233&lpg=PA233&dq=cabal+safe+unsafe+trustworthy&source=bl&ots=coVtmCo_7d&sig=ACfU3U2BIYic7oivXf15hZdr3dZASCL28A&hl=en&sa=X&ved=2ahUKEwjz6aHZjYnpAhWlURUIHeqsCHMQ6AEwAHoECAsQAQ#v=onepage&q=cabal%20safe%20unsafe%20trustworthy&f=false)
 * [Cabal Language Extensions](https://ghc.gitlab.haskell.org/ghc/doc/libraries/Cabal-3.3.0.0/Language-Haskell-Extension.html)

## Memes shelf

![Rank*Types](https://user-images.githubusercontent.com/4276606/81500658-8ffd9b00-92cb-11ea-87a2-5fabdde67a7c.jpg)
![GHC](https://user-images.githubusercontent.com/4276606/81500838-ad7f3480-92cc-11ea-80f2-b84daaff9586.jpg)
![Puns](https://user-images.githubusercontent.com/4276606/81501428-45325200-92d0-11ea-8783-6fd9c8e1f1bf.jpg)
![Rank*Types](https://user-images.githubusercontent.com/4276606/81501431-46637f00-92d0-11ea-94bb-8c7d45ae2080.jpg)
![Safe-Trustworthy](https://user-images.githubusercontent.com/4276606/81501434-46fc1580-92d0-11ea-8183-aa7939faa76b.jpg)
