---
title: "Haskell: Build Tools"
author: Dmitrii Kovanikov
tags: haskell, stack, cabal, build-tools, tutorial
---
# Haskell: Build Tools

This blog post describes the two main ways of managing and building Haskell
projects: using the `cabal` and `stack` build tools. The blog post doesn't try
to replace documentation for any of the building tools, nor does it try to cover
all possible ways to build a Haskell project. Rather, it tries to give beginners
step by step instructions on how to create and build a simple project. The goal
is to reduce the confusion, especially for those who have just started working
with Haskell. While documentation is good to gain a deep understanding,
sometimes you just need things to be done straight away.

## Introduction

Haskell is a compiled programming language that also has an interpreter. The
interpreter allows you to experiment with packages, like those from
[Hackage](https://hackage.haskell.org/) (Haskell central packages repository),
in a small single file or even in the interactive REPL called `ghci`. But for
big projects, it's better to organize the source code into modules as it will
help with maintainability. Build tools like `cabal` and `stack` can help you
manage the building process.

Hackage has a lot of libraries and finding the function you need can be
frustrating. Luckily, you can use [hoogle](https://hoogle.haskell.org/) to
search by name or type. Even then there are several build tools for Haskell and
deciding which one to use can be difficult. This blog post focuses on two such
tools:

1. `cabal-install`
    * Documentation: [https://www.haskell.org/cabal/users-guide/](https://www.haskell.org/cabal/users-guide/)
2. `stack`
    * Documentation: [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

These tools are chosen because they are the most popular and mature. I won't
make any claim on which one is better, instead I will give you an overall idea
of what the package development looks like for each of them. Hopefully, by the
end of this post you will be able to choose which approach fits you more.

## Cabal

The word `cabal` in Haskell is semantically overloaded. In Haskell,  `cabal` can refer to:

1. `cabal-install`
   * The build tool for Haskell projects.
2. Format of files with extension `.cabal`
   * The configuration (modules, dependencies, metadata) of a package
     `foo` must be written in a file called `foo.cabal` with a special syntax
     that can be read by `cabal-install`.
3. Haskell library [`Cabal`](http://hackage.haskell.org/package/Cabal)
   * This is a library used by `cabal-install` that implements a parser for the
     `.cabal` file format.

**PLEASE DON'T USE THE WORD "CABAL" WITHOUT CONTEXT!**

In this blog post when I use the word `cabal` I mean `cabal-install`. Otherwise
I will specify it explicitly.

### Installation

#### Ubuntu

The latest `cabal` and `ghc` can both be installed through `hvr/ghc` PPA:

* [https://launchpad.net/~hvr/+archive/ubuntu/ghc](https://launchpad.net/~hvr/+archive/ubuntu/ghc)

Use the following commands for installation:

```shell
$ sudo add-apt-repository ppa:hvr/ghc
$ sudo apt update
$ sudo apt install ghc-8.4.3 cabal-install-head
```

> **NOTE:** It's best to install `cabal` HEAD since it has better support for
> the commands we will use to build projects with `cabal`.

You can check the versions of the installed binaries like so:

```shell
$ /opt/ghc/bin/ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.4.3
$ /opt/ghc/bin/cabal --version
cabal-install version 2.3.0.0
compiled using version 2.3.0.0 of the Cabal library
```

To make development easier, you can also add those binaries to your `$PATH`
environment variable.

```shell
$ echo 'export PATH="$PATH:/opt/ghc/bin"' >> ~/.profile
$ . ~/.profile
```

#### Mac OS

To get `cabal` for Mac Os you can just use [`brew`](https://brew.sh/):

```
$ brew install ghc cabal-install
```

If you want to work with multiple GHC versions on macOS, you may find neat
`haskell-on-macos.py` script useful:

* [https://haskell.futurice.com/](https://haskell.futurice.com/)

#### Windows

The easiest way to install `cabal` and `ghc` for Windows is to install the
Haskell Platform, which you can do following the instructions at:

* [https://www.haskell.org/platform/](https://www.haskell.org/platform/)

As an alternative you can `chocolatey`, a package manager for Windows:

* [https://chocolatey.org/packages/ghc](https://chocolatey.org/packages/ghc)

### Project initialization

To create a new project you can run the `cabal init` command. Cabal will then
guide you through the interactive process, and ask you several questions
regarding the structure of your project.

```shell
$ mkdir cabal-example # create project directory
$ cd cabal-example    # go into this directory
$ cabal init          # initialize project in this directory
```

See demo of `cabal init` command below:

[![asciicast](https://asciinema.org/a/l4MYpsML6pAaJ0vwqKFxm4pgp.png)](https://asciinema.org/a/l4MYpsML6pAaJ0vwqKFxm4pgp)

As an alternative, you can use the
[`summoner`](https://github.com/kowainik/summoner) tool. `summoner` can generate
for you much more than what `cabal init` can, and it won’t ask you any redundant
question. However, you probably should just use `cabal init` for your first
Haskell project because `summoner` requires installation to be done first.

### Project structure

If you’ve chosen to create `cabal-example` with `src` as a directory

After initializing your package with name `cabal-example` which builds `Library`
and has `src/` as code source directory, you will get the following structure:

```shell
$ tree cabal-example
cabal-example/
├── ChangeLog.md
├── LICENSE
├── Setup.hs
├── src/
└── cabal-example.cabal

1 directory, 4 files
```

`cabal-example.cabal` file describes the structure of your package. Here is how
it looks like for our example project:

```cabal
-- Initial cabal-example.cabal generated by cabal init.  For further
-- documentation, see http://haskell.org/cabal/users-guide/

name:                cabal-example
version:             0.1.0.0
-- synopsis:
-- description:
license:             BSD3
license-file:        LICENSE
author:              Dmitry Kovanikov
maintainer:          kovanikov@gmail.com
-- copyright:
category:            Data
build-type:          Simple
extra-source-files:  CHANGELOG.md
cabal-version:       >=1.10

library
  -- exposed-modules:
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.11 && <4.12
  hs-source-dirs:      src
  default-language:    Haskell2010
```

Put your source code under the `src/` folder. Don’t worry about the `Setup.hs`
file as it is not needed most of the time. You can ignore it or even delete it.

### Building the project

To build your project, you need to have some code. Let's create a `Dummy.hs`
file inside the `src/` directory with the following content:

```haskell
module Dummy where

inc :: Int -> Int
inc x = x + 1
```

After that, let’s replace this line inside `cabal-example.cabal`:

```cabal
  -- exposed-modules:
```

with:

```cabal
  exposed-modules: Dummy
```

to tell `cabal` that this module is now part of your package.

Then, you will need to update the Hackage index, so that `cabal` is aware of the
most recent versions of the Haskell packages. To do so run this command from
your package root directory:

```shell
$ cabal new-update
```

You should run this command only in one of the following situations:

1. You’ve never run this command before.
2. You’ve deleted the `~/.cabal` directory.
3. You want to use a newer version of a library that has become available after
   you last executed `cabal new-update`.

> **NOTE:** If something goes weirdly wrong when building dependencies (like
> scary linker errors) then deleting the `~/.cabal` directory might help.

Finally, to actually build the package, run the command:

```shell
$ cabal new-build
```

> **IMPORTANT NOTE:** `cabal` also has a `build` and `update` command
> **without** the `new-` prefix. Don't use them! Use the commands with the
> `new-` prefix instead: they just work better. Implementing proper build tool
> is a really difficult task and it’s not immediately obvious how things should
> be done. But this doesn’t mean that once you figure out a better way of doing
> things you should implement it under the old interface. Package authors care
> about their users: they keep the old way to build packages so people can
> continue using it, even though the new proper way is available (through the
> `new-` prefixed commands). Old commands will be replaced by new ones in
> `cabal-3.0` and this prefix will be removed.

### Adding dependency

All dependencies should be specified under `build-depends` field in the `.cabal`
file. By default, every new package has the `base` library as a dependency. But
there are many more packages on Hackage! For example, to operate with random
values we can use the [`random`](https://hackage.haskell.org/package/random)
package. In order to do that you will need to modify the `build-depends` field
in the `.cabal` file in the following way:

```cabal
  build-depends:       base >=4.11 && <4.12, random
```

And then, we will be able to use any function from this package!

```haskell
module Dummy where

import System.Random (randomRIO)

inc :: Int -> Int
inc x = x + 1

dice :: IO Int
dice = randomRIO (1, 6)
```

Run `cabal new-build` to ensure that everything builds.

> **NOTE:** Usually there are multiple versions of a package with the same name.
> You may notice that version boundaries are specified for the `base` package in
> your `.cabal` file. If you have a lot of packages without explicit version
> boundaries, `cabal` might not be able to build your project, because it won’t
> know which versions of the packages to pick.

### REPL

Okay, we wrote some functions. So how about testing them? As mentioned earlier,
`ghc` has an interactive interpreter. You can run it with the following command:

```shell
$ cabal new-repl
```

And then you can evaluate the code we wrote in REPL:

```haskell
ghci> inc 64
65
ghci> dice
2
ghci> dice
1
ghci> :q
Leavning GHCi.
$
```

You probably won't see the same exact numbers as output for the `dice` function.
But there's a `1/36` chance you will.

### Adding executable

Evaluating functions in REPL is really fun! But usually programs are implemented
to be used as executables later. As I mentioned earlier Haskell is a compiled
language so you can have native binary. To do this you need to add `executable`
to the `.cabal` file.

> **NOTE:** Top-level sections like `library`, `executable` and others are
> called _stanzas_ in the cabal format specification.

Let's introduce an executable stanza by adding the following lines at the end of
our `.cabal` file:

```haskell
executable my-exe
  main-is:             Main.hs
  build-depends:       base, cabal-example
  default-language:    Haskell2010
```

> **NOTE:** We add `cabal-example` to `build-depends` to be able to use
> functions from our `library` stanza. No need to specify version bounds for
> `base` here since they are derived from the `cabal-example` dependency.

Now, we need to create the `Main.hs` file in the project root. You can put
anything you want as long as this file contains a function `main :: IO ()`. Use
your imagination! Mine is enough only for writing something like this:

```haskell
module Main where

import Dummy (dice)

main :: IO ()
main = do
  putStrLn "777"
  n <- dice
  print n
```

We gave our executable name `my-exe` so we can launch it by running the `cabal
new-exec` command:

```shell
$ cabal new-build # don't forget to build your project after changes!
$ cabal new-exec my-exe
```

Congratulations, you just run your first Haskell program!

For a more detailed but still beginner-friendly introduction to `cabal`, I
recommend this podcast:

* [https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html](https://haskell-at-work.com/episodes/2018-05-13-introduction-to-cabal.html)

## Stack

Even if you are only interested in using `stack` you should read the `cabal`
section first, as I will refer to it in some parts.

### Installation

You can find instructions for how to build `stack` at the beginning of the
`stack` documentation:

* [https://docs.haskellstack.org/en/stable/README/#how-to-install](https://docs.haskellstack.org/en/stable/README/#how-to-install)

You don't need to install `ghc` separately since `stack` will download a
suitable compiler version for you.

#### Unix

Installing `stack` on any unix system is really easy:

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
```

#### Windows

For Windows, you can download the official binary directly from the website:

* [https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

### Project initialization

To create a new project named `stack-example` you run:

```shell
$ stack new stack-example
```

> **NOTE:** To successfully finish the project initialization with `stack` your
> working machine should have an internet connection since offline mode has not
> been implemented yet.

Unlike `cabal init` command `stack new` doesn't ask you any questions. It just
creates the project according to a default template. However, you can specify an
existing template (or even create your own one):

* [commercialhaskell/stack-templates](https://github.com/commercialhaskell/stack-templates)

Alternatively you can use the [`summoner`](https://github.com/kowainik/summoner)
tool to create your projects. Any `stack` template describes a static project
hierarchy with only simple configurable metadata like _user name_. But sometimes
you might want more, like the ability to add `benchmarks` stanza to your
package. Generally, you don't want to have multiple templates that are just
slightly different versions of one major template. `summoner` can also
automatically create a GitHub repository for your new project, so it is
somewhere in between the `cabal init` and `stack` templates, but with some extra
features.

### Project structure

The directory hierarchy for the default `stack` template looks like this:

```shell
$ tree stack-example/
stack-example/
├── app/
│   └── Main.hs
├── ChangeLog.md
├── LICENSE
├── package.yaml
├── README.md
├── Setup.hs
├── src/
│   └── Lib.hs
├── stack-example.cabal
├── stack.yaml
└── test/
    └── Spec.hs

3 directories, 10 files
```

Here you see that many more files were created for you than with `cabal`. Here
are `library`, `executable` and `test-suite` stanzas added to the `.cabal` file
with the corresponding Haskell code being put into the `src/`, `app/` and
`test/` directories respectively.

The `stack.yaml` file contains some extra configuration for `stack`.

The `package.yaml` file contains the packages description in an alternative
format that is used by [`hpack`](https://github.com/sol/hpack). TL;DR Instead of
writing package description in `cabal` format you can write it in YAML and you
get some extra features like automatic modules discovery. `hpack` then generates
a `.cabal` file using information from `package.yaml`. But if you don't want to
deal with `hpack` for now you can just delete `package.yaml` file.

### Building project

Use the following command to build the project including `library`, `executable`
and `test-suite` stanzas, and run tests as well.

```shell
$ stack build --test
```

`stack` will automatically download the proper GHC version during your first build.

### LTS resolver

One of the main differences between `stack` and `cabal` is how they determine
the proper version for package dependencies. They both use the `Cabal` library
to parse `.cabal` files. However, `stack` uses the notion of LTS resolver. In
simple words: resolver is just a Hackage snapshot where packages can work with
each other smoothly. You can check `stack.yaml` file to see which resolver you
use. Look for a line like this one:

```haskell
resolver: lts-11.14
```

> **NOTE:** This is the only line in `stack.yaml` that is required for stack to
> work in most cases. The default `stack.yaml` file will contain a lot of
> redundant stuff. You can safely delete everything except this one line, unless
> you need some more advanced usage.

### Adding dependency

Adding dependency for `stack` is almost the same as for `cabal`: you just add
the library you want to the `build-depends` field.

If you are wondering which version of the package from `build-depends` will be
used in your project, you can open the Stackage web page with the resolver
specified in your `stack.yaml` and search for the library you're interested in.
For example, here we need

* [https://www.stackage.org/lts-11.14](https://www.stackage.org/lts-11.14)

You can also do this through the terminal. For instance, for the `random` library
we can run:

```shell
$ stack ls dependencies | grep random
```

Sometimes a library might not be in the specified resolver. In that case, you
need to add the library and its version to the `extra-deps` field inside
`stack.yaml`.
[See example here](https://github.com/kowainik/summoner/blob/21c4bbf2888b232ee8937e0d880cb438d9514d81/stack.yaml#L3-L5).

### REPL & executables

You can use `stack repl` command to launch the `ghci` inside your project.

Use the following command to run a specific executable stanza:

```shell
$ stack exec name-of-my-executable
```

## Conclusion

In conclusion, I want to say that workflows for `cabal` and `stack` might look
very similar, but the ideas behind them are different. I recommend you to try
both before sticking to one tool. And I hope the instructions above will help
you to get started with creating Haskell projects!

## Other

Useful addition to `cabal` and `stack` in terms of building tools is `nix`.
[Nix](https://nixos.org/nix/) is also a very popular choice but it's not yet
beginner friendly. If you are interested in knowing more about how to apply
`nix` to Haskell, I suggest you read this tutorial:

* [Gabriel439/haskell-nix](https://github.com/Gabriel439/haskell-nix)

If you're interested in more experimental ways to build Haskell packages, you
can check the following repositories:

* [phischu/fragnix](https://github.com/phischu/fragnix)
* [nmattia/snack](https://github.com/nmattia/snack)
* [mrkgnao/rien](https://github.com/mrkgnao/rien)
