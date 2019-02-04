---
title: "Hacktoberfest 2018 Wrap-up"
author: Kowainik
tags: haskell, hacktoberfest
---

Wow, what an exciting month it was! The open-source world completely took our
minds, hearts and time. But in exchange, it gave us 31 wonderful days of
collaboration with a lot of awesome people and we would like to thank them all!
Hacktoberfest 2018 is over, but it’s not the time to be sad or to say goodbye,
it’s only a time to collect all statistics and to share it with everyone else:

* 37 amazing volunteers contributed
* 96 outstanding pull requests in total were submitted
* 72 issues of various difficulty were resolved
* 5173 additions and 1646 deletions were bravely committed

## Highlights

If you like the statistics as much as we do or just would like to see more
detailed information about the changes, we have prepared the following sections
with the main highlights for each of the participating project and some numbers
to show our joint effort during the last month.

### relude

Our alternative prelude underwent several useful changes. The main ones:
reexporting `Contravariant` functors from `base`, various improvements to the
custom HLint rules, documentation enhancements and more convenient functions to
work with `Validation`, `Typeable`, tuples and much more.

![`relude` insights](https://user-images.githubusercontent.com/4276606/47835030-eb98bf80-dddc-11e8-8e19-7f279f804258.png)

### summoner

This project received various improvements to the configuration settings and
several bugfixes of different parts of project scaffolding. Also, the internals
of `summoner` were significantly refactored, improved and became more type safe.

![`summoner` insights](https://user-images.githubusercontent.com/4276606/47835189-96a97900-dddd-11e8-9361-a43c7f2717a7.png)

### co-log

Our logging library received very generous contributions! Among them
multithreading logger actions based on the bounded channels, various
improvements to the interface that make it more convenient and the `doctest`
checks to the core modules.

![`co-log` insights](https://user-images.githubusercontent.com/4276606/47835042-fb180880-dddc-11e8-9846-def75b71db11.png)

### tomland

Tomland is our bidirectional TOML conversion library. It hasn’t been publicly
announced yet, but a lot of people were helping us to achieve the `1.0.0`
milestone in a very short time. Our amazing volunteers added a lot of
improvements to the parser, bidirectional converters for various types, golden
tests for pretty-printing, benchmarks with other libraries and much more! This
really moves this library to a stable state.

![`tomland` insights](https://user-images.githubusercontent.com/4276606/47835169-85606c80-dddd-11e8-9bbd-1686ac31db0a.png)

### containers-backpack

A lot of people did a great job on improving Backpack interface for
`containers`: faster implementations of functions using smart techniques,
implementation of a basic polymorphic test-suite with the property-based tests
for laws and an initial version of benchmarks using the Backpack signatures.

![`containers-backpack` insights](https://user-images.githubusercontent.com/4276606/47835153-71b50600-dddd-11e8-804d-cb56a6c24705.png)

### typerep-map

Our fast dependent map now has benchmarks for the `insert` functions and the
work on speed boost of this function is in progress.

![`typerep-map` insights](https://user-images.githubusercontent.com/4276606/47835049-04a17080-dddd-11e8-9338-2ec5e5b72375.png)

### smuggler

GHC Source Plugin that automatically removes unused imports now can remove
trailing commas in imports! This was a problem because trailing commas in
imports lists [leads to the compilation error](https://ghc.haskell.org/trac/ghc/ticket/15392),
but now it works smoothly. Also, the performance of the plugin was improved and
more tests were added.

![`smuggler` insights](https://user-images.githubusercontent.com/4276606/47835057-09febb00-dddd-11e8-9ab3-65f461068cc2.png)

## Acknowledgment

This is a huge amount of work, and we are really proud that so many wonderful
people come to help our small organization to produce better libraries.

We would like to thank everyone personally who submitted improvements to our
repositories. Here are the names of our heroes:

* [achilleasNP](https://github.com/achilleasNP): 1 PRs
* [AlexBoliachiy](https://github.com/AlexBoliachiy): 1 PRs
* [arkrost](https://github.com/arkrost): 2 PRs
* [astynax](https://github.com/astynax): 5 PRs
* [b-mehta](https://github.com/b-mehta): 1 PRs
* [Bargsteen](https://github.com/Bargsteen): 1 PRs
* [brandonhamilton](https://github.com/brandonhamilton): 1 PRs
* [cdeepanshu](https://github.com/cdeepanshu): 1 PRs
* [Cmdv](https://github.com/Cmdv): 2 PRs
* [cocreature](https://github.com/cocreature): 1 PRs
* [cronokirby](https://github.com/cronokirby): 12 PRs
* [FintanH](https://github.com/FintanH): 1 PRs
* [gahag](https://github.com/gahag): 3 PRs
* [guibou](https://github.com/guibou): 1 PRs
* [JasonMFry](https://github.com/JasonMFry): 1 PRs
* [jiegillet](https://github.com/jiegillet): 7 PRs
* [Jonathas-Conceicao](https://github.com/Jonathas-Conceicao): 1 PRs
* [kahlil29](https://github.com/kahlil29): 1 PRs
* [karen](https://github.com/karen): 1 PRs
* [Leschonander](https://github.com/Leschonander): 1 PRs
* [lucazulian](https://github.com/lucazulian): 1 PRs
* [mauriciofierrom](https://github.com/mauriciofierrom): 1 PRs
* [MitchStevens](https://github.com/MitchStevens): 2 PRs
* [pascalpoizat](https://github.com/pascalpoizat): 1 PRs
* [piq9117](https://github.com/piq9117): 4 PRs
* [qnikst](https://github.com/qnikst): 6 PRs
* [qoelet](https://github.com/qoelet): 7 PRs
* [sam46](https://github.com/sam46): 1 PRs
* [Simeon979](https://github.com/Simeon979): 3 PRs
* [sphaso](https://github.com/sphaso): 4 PRs
* [StevenXL](https://github.com/StevenXL): 1 PRs
* [tfausak](https://github.com/tfausak): 1 PRs
* [thomaspepio](https://github.com/thomaspepio): 1 PRs
* [tomphp](https://github.com/tomphp): 1 PRs
* [v0d1ch](https://github.com/v0d1ch): 5 PRs
* [willbasky](https://github.com/willbasky): 4 PRs
* [yigitozkavci](https://github.com/yigitozkavci): 2 PRs

## Future

This was a challenging month for us as for maintainers as well. To be honest, we
didn’t expect that many amazing contributions by a lot of awesome people! We are
really happy that people did so many useful and helpful things. We were trying
to spend all our time to make the PR submitting/reviewing process as clear, easy
and fun as possible. We tried our best to provide good feedback through help and
code review, we devoted this whole month to Hacktoberfest and we hope that
people really enjoyed working with us and learning new stuff 😊

Anyway, if you have some feedback or thoughts on how to improve participants
experience in the future, let us know! We’re always open for ideas that help to
make the world better 😃

![Our happy maintainers](https://user-images.githubusercontent.com/4276606/47839725-b5fcd200-ddee-11e8-92a3-9b3bca7a7447.jpg)
