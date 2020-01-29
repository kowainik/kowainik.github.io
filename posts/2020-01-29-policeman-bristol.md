---
title: 'Policeman in da Bristol city'
author: Veronika Romashkina <> Dmitrii Kovanikov
tags: haskell, tool, GHC, PVP, cabal
description: 'Bristol Haskell Hackathon 2020 project — Policeman — a tool for Haskell PVP version suggesting.'
useShortName: yes
---

## Bristol Hackathon

We have just got back from the first-ever hackathon we participated in, that
took place on [25-26 January 2020 in Bristol](https://mpickering.github.io/bristol2020.html).
And we can't wait to share our experience, since (good news) we have something
to present you as the result of our productive and fun weekend!

But first, we would like to say thanks to the organizers of the event who
brought together so many amazing Haskellers in one place. This event was
fruitful on unexpected collaborations and lots of productive hours of work on
diverse types of projects.

In this post, we want to demonstrate the output of our work at this Hackathon —
the idea we had in mind for a while but did not manage to bring to life. Please,
meet _Mr Policeman_!

 * [kowainik/policeman](@github)

## Policeman

In Kowainik we strive to help the Haskell ecosystem and produce beneficial
libraries and tools. That is why for the two-day long Hackathon we set a goal to
work on a project which helps both novices and experts. This time we decided to
help ease the burden of package maintenance as this topic is of high interest to
us — we deal with it on a daily basis and have firsthand experience in the
process challenges. The project we were working on is called `Policeman` and it
assists to properly choose the next version number for the Haskell packages
based on the semantic changes in the exposed interface. But before we dive into
details, let's first talk about the official versioning policy to better
understand the need in such an instrument.

The recommended practice for Haskell libraries is to follow
[PVP](https://pvp.haskell.org/) — Package Versioning Policy — a policy designed
to handle Haskell specifics especially well. You can use other versioning
strategies if you prefer, nothing stops you from doing so at any stage of the
release cycle, however, different pieces of the Haskell ecosystem interact with
each other more smoothly when they embody the single established format — PVP.
Following the determined policy can prevent some unfortunate version
incompatibility nightmares between libraries, as they depend on each other in so
many ways.

PVP defines a [set of rules](https://github.com/haskell/pvp/blob/master/v1.1/pvp-specification.md)
of how you must change your package version in accordance with the changes in
the code. Simply speaking, versions in Haskell should have a form of **A.B.C.D**
where the components stand for the following meaning:

 - **A** — marketing major version; usually changed when a package had
   significant breaking changes
 - **B** — major version, should be bumped up on breaking changes
 - **C** — minor version, updated on non-breaking changes
 - **D** — tiny minor version, updated on bug fixes, documentation patches, etc.

Let's examine some example situations. In the following stories, our current
version is **1.2.10.3**.

 🔍 You updated the documentation, then your new version might be **1.2.10.4**

 🔍 You added a new function, then your next release version should be
 **1.2.11.0**

 🔍 You deleted one of the functions exposed by your library, then you should
 assign a new version **1.3.0.0**

 🔍 You rewrote half of your library or introduced a new significant feature,
 then it might be a good idea to create a new major release with the version
 **2.0.0.0**

If you browse various packages on [Hackage](https://hackage.haskell.org/), you
may notice versions that contain a different number of components with some
components skipped or added, like 1.0 or 2.0.3 or
[3.6.1.0.1.0.0.1](https://hackage.haskell.org/package/cctools-workqueue-3.6.1.0.1.0.0.1).
Even though it is not a problem for Haskell build tools since all versions can
be ordered, the recommendation would still be to specify all four components for
the consistent versioning scheme and avoidance of some tricky version
collisions.

Official PVP page contains an exhaustive set of rules you can follow to properly
change the version of your package. However, it is still possible to make
mistakes, since the process is manual and complicated, therefore error-prone.
And, what is more important, when you make a mistake, sometimes you can realise
that only after releasing the package which makes the cost of the mistake
extremely high. Thus, there is a need to automate this process and that is why
we have created `policeman` — your versioning guard.

In the following section, we are going to describe our implementation flow and
usage of the modern advances in Haskell tooling to achieve our set goal.

## Briefing

The PVP repository contains an
[algorithm in the form of a flowchart](https://github.com/haskell/pvp/blob/master/v1.1/pvp-decision-tree.svg)
which defines how the version should be assigned. However, its description is
too high-level and declarative to be programmed in a straightforward way. In
view of that fact, we had to come up with a more detailed plan on how to find
semantic differences between the current and previous version of a package.
Below you can find the description of how Policeman actually performs its
duties:

 1. Parses the local cabal file to get all necessary information for the proper
    work of the tool: package name, version, list of exposed modules, etc.
 2. Uses the Hackage API to get the latest (or any other manually specified
    published) version of the package with the corresponding name.
 3. Downloads the tar archive of the previous version into a local temporary
    directory and unpacks it.
 4. Compiles both the current and downloaded versions to create the HIE files
    (will be covered more in the next section). This is the step that can be
    done only with GHC-8.8 or higher.
 5. Retrieves the information about both packages from their `.cabal` and
    generated HIE files.
 6. Finds the semantic diff of the exposed interface between the previous and
    current versions.
 7. Evaluates the diff to make a decision regarding the advised new version.
 8. Produces detailed output of all weighty semantic changes that affect the
    conclusion.

You can see that the implementation involves a lot of steps of various
difficulty that touches different areas — from dealing with infrastructure to
writing pure languages analysis algorithms. However, being highly declarative
language, Haskell allows us to implement the above plan in a modular way. With
the usage of the `ExceptT` concept we managed to create a nice architecture where
all that different components work smoothly together and we can get the control
of error handling in a convenient and extensible way in terms of the described
system.

Furthermore, we are separating pure logic as per the Haskell best abilities. The
"pure" stages such as diff building, result evaluation and versions
manipulations are completely isolated. That gives us the opportunity to provide
reliable testing (both unit and property) which could be useful in feature
shipping in the future as well.

Eventually, we were able to achieve MVP state of `policeman` in just two days.
Below you can find an example of the Policeman's work on some local changes for
the [relude](@github(kowainik)) package:

![Policeman example](https://user-images.githubusercontent.com/4276606/73396836-1fada980-42da-11ea-8c59-64fa985f54bc.png)

## HIE there

Initially, we were considering to parse source code of the Haskell files and
compare abstract syntax trees to detect the semantic difference between the
given two versions. Unfortunately, this approach is not capable of handling some
edgy cases, so it won't be able to work with some packages. That is the reason
why we decided to go with the HIE files as a source of truth for Policeman in
the long run. _We are not interested in fake evidence_ 🕵️.

[HIE files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html) are so
useful that we decided to dedicate a whole section to them and to their usage in
our project, and the role in Haskell tools of the future. They were introduced
in GHC-8.8 with the goal to help IDEs and Haskell tooling in general. For
example, [ghcide](@github(digital-asset)) already uses HIE files to provide
smooth IDE experience. HIE files contain syntactic and semantic information
collected by GHC during compilation about Haskell source code files, which can
be used for various analysis purposes. Thereby, instead of parsing source code
by specifying all required options from the `.cabal` files, dealing with CPP and
custom preprocessors, collecting location info from dependencies, one can just
take all this information from the files created by GHC specifically for the
purposes of making the Haskell tooling writers' lives easier.

HIE files are binary files with the `.hie` extension. They are not generated by
default, in order to produce them you need to pass
[`-fwrite-ide-info`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#ghc-flag--fwrite-ide-info)
flag to GHC. By default, they are put into the local build directory, but you
also can use the
[`-hiedir`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#ghc-flag--hiedir%20%E2%9F%A8dir%E2%9F%A9)
option to specify the output directory (which we do in `policeman`). Since these
files are binary, you cannot easily open them in a text editor and immediately
understand their content. You can use the [HieDb](@github(wz1000)) tool to query
information about those files. If you want to analyze their content
programmatically, you can add GHC as a library to the dependencies of your
project and decode HIE files by a function call. In Policeman, we go for the
second option and parse the content to extract the relevant information.
Specifically, we are interested in the exported data types and functions and AST
of exposed functions. Fortunately, HIE files provide this information in an
easy-to-work-with way.

For now, Policeman builds the current and the previous versions of your package
to generate `.hie` files. But we hope that the process of automatically creating
GHC-generated IDE information will become more common and those files will be
available immediately for processing. And we are looking forward to helping this
feature become more popular.

## Case closed

In the end, we want to add a few words about the Hackathon experience generally.
A dedicated hackathon is a wonderful place to focus on something that you wanted
to build for quite a long time. We managed to finalise the idea, create a plan
for work and finish working MVP at this event. Quite productive!

As a pleasant bonus, you get to see a lot of people sharing your ideas and
debate on decisions, tasks, and overall Haskell. So do try to get to one of such
events. Hope to see you on the following Haskell Hackathons, cheers!
