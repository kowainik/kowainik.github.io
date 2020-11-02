---
title: "Brave New Hacktoberfest — Learn4Haskell"
author: Kowainik
tags: haskell, hacktoberfest
description: "Results of the Hacktoberfest 2020 in Kowainik"
useShortName: yes
---

This blog post is our experience report and summary after working on
[Learn4Haskell](@github(kowainik)) — an educational project that took
place within Hacktoberfest2020.

It was an exhilarating time for us, and we hope that participants feel
the same way. We learned valuable lessons about the event mechanics
with our new approach to it. Moreover, we discovered lots of
interesting insights that could be helpful and interesting to everyone
involved or those who are planning to start similar courses for
Hacktoberfest.

In this writing, we are going to describe the GitHub course project we
offered to work at as a part of Hacktoberfest, share our journey as
maintainers and mentors, identify problems that beginners experience
with the course based on our practice of reviewing their
solutions. And we will also discuss some moral sides of experiencing
such an adventure.

Additionally, we describe pros and cons of using Haskell in the
educational field: high-grade parts that make developing and teaching
in it enjoyable, as well as some language shortcomings that make
newcomers (and sometimes educators) struggle with the language.

The blog post might be interesting for people who are

 * curious about open-source education, GitHub project courses
 * looking for ways to make "known as difficult-to-learn" tools more
   accessible
 * probably compiler developers and language designers who would love
   to make the programming languages more approachable
 * interested in creating similar courses in the upcoming
   Hacktoberfests
 * interested to see statistics (lots of numbers and charts!)
 * and everyone else who would love to hear the story from our side of
   the barricade!

## Brave New Hacktoberfest

Hacktoberfest is an annual event to encourage open-source
contributions. This year it is presented by
[DigitalOcean](https://www.digitalocean.com/),
[Intel](https://hacktoberfest.digitalocean.com/intel.pdf) and
[Dev.to](https://github.com/forem/forem).

We participate in Hacktoberfest steadily four years in a row, and this
year is no exception! For 2020 Kowainik prepared a special project
that puts together a lot of thoughts and goals we have for our
organisation. We are happy to present to you Learn4Haskell — a
beginner-friendly Haskell course.

  * [kowainik/Learn4Haskell](@github)

The idea behind Learn4Haskell is simple yet absolutely novel for
Hacktoberfest. Usually, for this event, lots of incredible people
donate their time and forces to improve some open-source projects. And
this is a beautiful initiative!

Most of the time during the year developers work on private work
projects, and Hacktoberfest is a perfect opportunity to pay back to
communities that produce open-source stuff that is used
anywhere. However, this year there were fewer opportunities to do that
as Hacktoberfest became an opt-in event. Moreover, contributing to
other projects is not an easy activity and could require much more
time and mental efforts to get a job done.

So we came up with the idea to utilise the hype around Hacktoberfest
and use it for a good deed. Instead of trying to make people
contribute to _our_ projects, we've offered them a way to spend that
time on _their_ self-improvement and education.

To successfully complete Hacktoberfest, you need to submit four valid
__pull requests (PRs)__. Thus we designed a 4-step course where you
can learn Haskell basics and finish Hacktoberfest simultaneously.

The course is a complete working Haskell project. At the same time, it
is split into four independent chapters — four source code files,
which makes it perfect for GitHub PR system. Each part is a working
module that contains learning material and tasks you need to solve to
pump your knowledge. Our chapters describe Haskell from basic syntax
and simple functions to Monads. The course doesn't require reading any
other materials or guides. So you can just open a source code file in
your favourite editor and start learning. Nothing else is needed
except the willingness to explore Haskell!

## Goals

While working on education material, it is easy to get lost and to
overcomplicate it (or make it uninteresting to follow). We aimed to
find the perfect balance between the minimal necessary content to
fulfil the basic essential knowledge for __Functional Programming
(FP)__ paradigms and the right amount of work for people to be able to
finish it by working on the course only a few hours a day.

> Learning Haskell should be easy and straightforward.

Keeping all that in mind, we created the Learn4Haskell project in pursuit of the following goals:

  * Help to get into the Haskell language.
  * Help to think in the Functional Programming way.
  * Challenge people to get out of their comfort zone of imperative
    programming and tweak their minds to accept FP paradigms.
  * Give a beginner-friendly and self-consistent course with theory
    and practice in the same place.
  * Explain Haskell topics before each task, but strive to be concise
    and useful at the same time. It's a tough balance!
  * Help people who are eager to participate in Hacktoberfest and
    Open-Source, but also want to learn new things during this
    process.
  * Provide review and feedback to solutions, so people are never
    alone in this challenging yet exciting journey.
  * Give people who completed this course all the necessary
    understandings to be able to work with basic projects that use
    standard features. We also intend to give a strong basis that
    would be enough to continue their functional programming studies.
  * Show the wonderful world of FP to people outside and prove that
    everyone can learn those concepts in no time.
  * Engage people to the Haskell community and make them feel welcome.

## Preparations and Execution

Though the idea and goals look straightforward, it required a lot of
time and effort from our side to get the project done. Besides the
only apparent part that we were doing during the Hacktoberfest itself
— reviewing —- we spent quite some time creating a competent project
that would fulfil our goals. Our work on Learn4Haskell was, in fact,
made in two giant, independent and essential steps:

1. _Preparation_: What needed to be done __Before__ the course.
2. _Execution_: What we need to do everyday __During__ the course (and
   __after__ as well).

### Preparations

First of all, we started with the course plan, materials and logistic
arrangements. We want people to be able to follow all the guides
easily. Moreover, the course itself must be easy to set-up. And we
also want it to bring something beneficial and valuable for people.

So, our preparations included the following actions:

* __Course agenda__. This is the toughest and the most
  time-consuming part. We've spent a lot of time structuring our
  tutorials, rethinking the topics that need to be covered,
  brainstorming various ideas and evaluating the difficulty levels.
* __Write the course content__. After deciding on the alignment of
  topics, we needed actually to write down explanations of different
  Haskell concepts. As we provide all information in code comments,
  we want to make it easy to follow visually. After writing the
  guides, we came up with the tasks that reflect the course
  materials. We've spent around a week only purely writing the
  content of the course.
* __Test the course__. Next step involved a lot of reviewing rounds,
  putting ourselves into the shoes of FP beginners and
  battle-testing course concepts and tasks against different
  aspects. We also needed to estimate time that people could afford
  to spend on the course. We believe that people don't have to spend
  too much time reading hundreds of Haskell guides in order to get
  started with the language, so the challenge was in keeping
  explanations concise but helpful simultaneously.
* __Configure the automatic testing environment__. Although we
  provide quick and detailed feedback regarding code, solutions,
  styling, ideas, performance and other aspects of the code
  structure and general advice about code in Haskell, we wanted to
  give people the ability to test their solution locally as
  well. Our course contains a lot of tasks of various difficulty,
  themes and concepts, and it makes sense to provide simple ways to
  check solutions for satisfying basic requirements. Since the
  course is aimed for beginners, who are not expected to know how to
  use Haskell build tools efficiently (which is tough to learn even
  for advanced Haskellers), we created two ways of testing
  solutions: GitHub Actions CI to test solutions when submitting PRs
  and Makefile with commands to test solutions locally. CI for forks
  can be enabled in one-click, so it is super easy to add automatic
  testing for your own repositories.
* __Prepare solutions__. As we want Learn4Haskell to be accessible
  to more people, we also understand that some would be shy to reach
  out to us, or ask for the review and ask questions. There could be
  lots of reasons for which people would prefer to work on the
  course standalone. And we definitely want learners to be able to
  verify their solutions even if they decide to go this learning
  journey on their own. So we've spent some time writing idiomatic
  solutions for all tasks that they can use in their learning.
* __Write documentation and instructions__. This is a tricky part as
  Haskell is known as a very-hard-to-get-started-with language. Our
  goal was to provide batteries-included instructions that are easy
  to follow, and that would work on all of the most popular
  platforms with minimal efforts. So we carefully describe
  get-started installation instructions, as well as the essential
  information about the course, how it works, what to expect and
  what participants should do on each step, and all other required
  metadata.
* __Design work__. As we wanted our project to be visible and
  Hacktoberfest compliant, we've spent some time making it look like
  candy. For that, we created a resonant name and branded around
  it. The Learn4Haskell naming was perfect for that as it
  demonstrates everything this project is about and greatly fits the
  4-steps course and 4 PR workflow of the event. Fortunately,
  Hacktoberfest helped to come up with the design, and the pull
  request sign perfectly represents number 4 in our
  logo. Hacktoberfest provides some templates, but it is still not
  an instant process to come up with the project name, unique
  concept, design and actually illustrate everything.

### Execution

After we've finished the work on the course material and announced it,
the second exciting phase comes into play. People submit their pull
requests, and we provide our feedback on their solutions. Our duties
include:

  * __Spreading the word__. It doesn't make sense to spend time
    creating a course if nobody tries it. So we worked hard
    encouraging people to give Learn4Haskell a go. This is a separate
    big job, which we were hoping to get more help from the community,
    but mostly we were doing it on our own, especially on the later
    stages of the course. Twitter was our primary tool to promote
    Learn4Haskell and provide updates. We tried Reddit and Discourse
    as well, but it did not help a lot. Of course, we can't force
    anyone to share things they don't want, so no hard feelings to
    anyone in particular! And we are grateful to everyone who decided
    to support us and share the course with friends and others!
  * __Reviewing__. This was our killer feature of the course, so we
    were glad that lots of people who submitted their solutions asked
    us for a review. And we did our best to provide high-quality
    reviews for each and every PR. Even though we saw hundreds of
    solutions to the same tasks, it still was fun and exciting. We
    noticed that some tasks are more error-prone, and they usually get
    the same types of mistakes from time to time. But it was important
    for us to look at every solution carefully, as for some people
    it's their first contribution, and we wanted to make sure that
    they really learn and understand the new concepts! We didn't want
    to let them down. Thus even our thousandth review kept the same
    high bar of quality as the first one.
  * __Answering questions__. Sometimes people struggled with some
    topics, and that is exactly what we expected and wanted to help
    with when providing our helpline anytime anywhere. People asked
    their questions through the PRs, code or as the replies to our
    comments, so we could elaborate more. Lots of questions were
    unique and thoughtful, and we were happy to see that, as it
    indicated that people are really eager to learn new ideas and
    don't mind listening to some advice. So we spent the time
    explaining Haskell concepts, sometimes even with ASCII-art and
    helpful code examples.
  * __Side notes and additional Haskell concepts__. When we saw
    opportunities for that, we tried to add some additional
    information in the review in the form of links to external
    resources or more elaborate explanations. Sometimes things were
    not covered by the course itself, but if we notice that it could
    be interesting for a person or could benefit people's ideas even
    more, we shared extra insights into the language usage.
  * __Reward good work__. Our review was not only about spotting
    problematic or weak points, but generally evaluating people's work
    and motivating them to take all from the course. Luckily for us,
    there were tons of exciting and thoughtful solutions, so we were
    not greedy in highlighting people's fantastic work!
  * __Tooling support__. Developers, who attempted to solve our
    course, use different operating systems — Windows, macOS, Ubuntu,
    Arch, Pop!_OS, NixOS, etc. Even though most of the time our
    instructions worked as expected, sometimes participants had issues
    with the Haskell tooling. We tried to do our best to share
    workarounds and possible solutions, so they can continue the
    course without problems.
  * __Troubleshooting__. We did the most we could in a limited amount
    of time to verify the course beforehand. But sometimes tasks
    descriptions weren't clear for everyone, or they contained some
    typos and a few other minor problems. Also, Hacktoberfest rules
    changed after several days. So we spend our time polishing all
    problems, warning people about changes in rules, answering their
    questions about the course, investigating errors, and much more.
  * __Collecting stats__. In our first Hacktoberfest, we shared some
    contribution stats, because a lot of people contributed to our
    projects, and we wanted to show interesting information. This year
    we planned stats gathering beforehand, and we've been collecting
    various contribution information to draw pretty plots and
    visualise contribution stats.
  * __Writing this blog post__. Compiling an experience report and
    summarising our thoughts to share our results is also not free in
    terms of time, notably for non-native English speakers. But we
    believe that it could help somebody in future, to be aware of all
    aspects of the event.

<hr>

You can see that creating a course and teaching it includes completely
exceptional tasks that require a diverse set of skills (both technical
and soft). Especially if you want not only to produce a decent and
marvellous course and leave your users happy but also make it possible
to maintain it easily and not go crazy.

## Stats

Without further ado, let's move to exciting results of the several
months of our work and see some statistics and visualisations!

Short summary of contributions:

  * __419__ people forked the Learn4haskell repository
  * __326__ stars on GitHub
  * __187__ people submitted their solutions to Learn4Haskell tasks and asked us for review
  * __67__ people finished all four chapters
  * We reviewed __481__ Pull Requests

<hr>

It was quite a month for us at GitHub, very dark-green. You can see
the total number of reviewed PRs for each of us from the following
screenshots of GitHub stats:

![PRs reviewed 1](/images/posts/hacktoberfest2020/prs-reviewed-vrom911.png)
![PRs reviewed 2](/images/posts/hacktoberfest2020/prs-reviewed-chshersh.png)

Our reviews contain various suggestions, and people patched their code
after our clues. So we re-reviewed PRs for the second, third and
sometimes the fourth time. The total number of reviews we provided is
approximately the above number of PRs multiplied by three.

<hr>

We were curious about how many people get started with our course each
day, so we've been collecting info about _the first open Pull
Request_, and here this information aggregated by days:

![Course starters per day](/images/posts/hacktoberfest2020/chart_started.png)

Interestingly enough, we had not a single day without new people. We
noticed some more activity on weekends, but generally everyday we
received several PRs with different Chapter solutions. Sometimes our
notifications were not decreasing even after clearing up some of the
PRs, because people were already submitting a few more, while we were
focused on one.

<hr>

Some people rapidly were nailing all four Chapters, others were active
only periodically. However, not everyone finished all chapters. Here
is the diagram of the PRs opening distribution per chapter.

![Solutions per chapter](/images/posts/hacktoberfest2020/chart_chapters.png)

As you see, there is a more significant gap between the second (lists
and streaming functions) and third Chapters (ADTs, typeclasses), so we
need to work on the problem to make it flatter. That is why we would
appreciate the feedback from people who haven't finished all Chapters
yet whether it is because of the lack of interest, time or difficulty,
we want to hear from you!

<hr>

In our course, we recommended people to open PRs to their own forks of
our course, but many people submitted PRs to our repository (that was
not a problem for us, though we will talk about downsides later). Here
is a pie chart for this information:

![Fork vs Repo](/images/posts/hacktoberfest2020/chart_fork.png)

You can see from the chart that more than half of the people opened
pull requests to their own forks, though some people submitted their
solutions directly to the course repository. We are going to talk
later about the difference in review workflow between forks and our
repository.

<hr>

A lot of Learn4Haskell participants helped us make the course better
by improving documentation and adding more test cases. Here is a chart
of the relationship between all participants and active contributors:

![Contributors diagram](/images/posts/hacktoberfest2020/chart_contributors.png)

<hr>

It's interesting to see charts of stars growth and distribution of
stars per location.

![Stars trend](/images/posts/hacktoberfest2020/chart_stars.png)

![Stars location](/images/posts/hacktoberfest2020/chart_star_location.png)

<hr>

From the stats, you can see that many people tried Learn4Haskell. Some
people helped us to improve the course both course participants and
non-participants.

Almost 200 people tried our course (and these are only people who
asked us for review, the total number of forks is twice as big!). But
because of GitHub UI, it wasn't easy for people to open pull requests
to their forks, so many of the PRs were open to our course's
repository.

Not all people finished all four chapters. According to the
distributions of submitted solutions per chapter, the third chapter
seems to be the most challenging (the gap between chapters two and
three is bigger). In Chapter Three, we explain custom data types, sum
types, newtypes and typeclasses. And, indeed, from our observations,
we conclude that people struggle the most with the type system itself,
not with specific typeclasses.

We hope that our stats are useful (or at least interesting) to
everyone, and there's a lot to learn from them!

## Tools advantages

To get more out of the platform and tools we are using, we take all we
can from the nicely provided features of the Haskell language, Haskell
compiler and GitHub platform.

Haskell and its ecosystem have several exceptional qualities that make
such project-based all-inclusive courses shine. Some remarkable
Haskell features, wonderful tooling around the language created by a
lot of enthusiasts and GitHub ecosystem perks made it very easy for
people to start coding in Haskell and not even thinking that this is
elitism or privilege!

Basically, to get started with Haskell via our course, you just

a. Install compiler and the build tool with a few commands.
b. Open your favourite editor (or install VS Code with the brilliant
   all-inclusive Haskell plugin).
c. Run Haskell REPL and load a module you're currently editing.

That's all!

We highlight the main benefits of various tools we used in our course
to provide a smooth experience for participants:

1. __GHCi__. REPL is an incredibly powerful tool when learning a new
   programming language. You don't need to create a complete project
   or even produce a binary file to run your code. You can just write
   a function and test it immediately in REPL. The feedback cycle is
   very short!
2. __Haskell tooling__. The state of IDE in Haskell improved a lot
   recently! It is very easy to have a full-featured, stable and
   working Haskell development environment anywhere
   nowadays. Configuring the Haskell tooling is relatively
   straightforward everywhere: install (1) ghcup
   (ghcups on Windows), (2) VSCode, (3) the Haskell plugin, and that's all! The VSCode's
   Haskell plugin automatically downloads all extra tools for you, so,
   if you have a good internet connection, you can start coding in
   Haskell in minutes. In our course, we provided instructions on how
   to install Haskell, and so far, a small list of steps was enough
   for people to get started!
3. __Documentation testing__. We use comments to structure our
   tutorial and all presented information. Our explanations contain
   multiple code snippets, and we want to make sure they are
   correct. We used the brilliant Haskell testing library `doctest`
   for this. It allows testing such documentation examples. So, on the
   one hand, we are sure that we don't provide bogus
   code. Additionally, learners can verify that their implementations
   pass basic documentation examples. Moreover, we added playing in
   REPL as the first exercise, and people can verify what they see
   through our tests. Even though `doctest` is not perfect in some
   aspects, it turned out to be very helpful!
4. __Hintman__. In Kowainik, we developed a
   [GitHub Application](https://github.com/kowainik/hintman) that runs
   the Haskell linter HLint on source code diff of the PR and provides
   an automatic instant review. Some people told us that our app
   already provided helpful suggestions for writing better Haskell
   without even our manual review! Just a screenshot of one such
   example:

   ![Hintman](/images/posts/hacktoberfest2020/hintman-thanks-bot.png)

5. __CI in one click__. Having an easy way to run various tests and
   notify participants of the results without manual review is a great
   advantage. This is possible due to the wonderful GitHub Actions and
   Haskell setups for that.
6. __Haskell libraries for the unit- and property-testing__. We use
   Haskell libraries `hspec` for unit testing and `hedgehog` for
   property testing. It turns out, tests in Haskell are easily
   readable and writable, and multiple people were able to add their
   own tests without even learning about Monads, do-notation and many
   other things.

## Beginner struggles

Our course is intended for beginners in Haskell or open-source in
general. Despite all advantages of used technologies, people still
struggle with some parts due to the legit reasons and actual
problems. Let's see them and think about how it could be improved.

### General

Our course is located on GitHub, as we prepared it for
Hacktoberfest. Notwithstanding all the nice bonuses it gave to boost
our course, we spotted a few things that could have been improved in
order to make the experience of first-time contributors better, as
well as help maintainers with more comfortable and more agreeable
processes.

1. __Fork pull requests__. We recommend people to open pull requests
   to their forks. This way, they have more freedom, can merge their
   solutions and enjoy all green CI in the end when all checks
   pass. However, a lot of people (as you saw in stats) opened PRs to
   our repository. And many were confused about how to open PRs to
   their forks. Not to mention that hundreds of PRs were just open by
   mistake in the wrong place due to the non-convenient design of
   branch/repo choice when opening PRs!

   To elaborate more on why we prefer the forking workflow for the
   course, here are some troubles with opening PRs to our repository:

    1. Confusion/additional work for our participants on how to
       separate solutions chapters into different PRs; it is actually
       just another thing to watch out for or be worried about.
    2. Our automatic reviewer `hintman` sometimes produces noisy
       suggestions that we don't actually want to expose to people on
       that point of the study journey (as they are a bit more
       advanced, or it expels the solutions). Hintman also warns about
       trailing whitespaces, so it was more difficult and
       time-consuming to review people who haven't set up trailing
       whitespace cleanup in their IDEs.
    3. We had to close PRs afterwards, which is always mentally hard
       and sometimes can feel rude.
    4. We had to deal with labels. The price of us forgetting to put
       them on PRs (which is very easy with that many PRs) is
       someone's Hacktoberfest prize, and we definitely don't want
       that!
    5. It is usually double work for us in case of the 'oops' pull
       requests: one in our repo + one in their fork. And people do
       not always close the outdated ones, so we need to double-check
       notifications to make sure this is not the case, which is also
       time-consuming.

2. __Visibly hidden review comments__. When a review contains a lot of
   messages, some of them become hidden by the GitHub UI. They are
   often hard to find, so people sometimes don't even see parts of our
   review.
3. __Line-breaks__. Some people develop on Windows, and when they save
   files, editors change line-breaks on every single line. This makes
   diff hard to read at first. Fortunately, there's an option to
   ignore whitespace changes.
4. __Misleading update notifications__. When you need to review dozens
   of PRs daily, the UI around notifications and updates plays a
   crucial role. When jumping to a PR from the notifications tab, it's
   often not clear immediately what's actually changed: is it a new
   comment, a new commit or just a label addition? Especially in
   hidden or resolved comments, there is no way to see if anything was
   changed in there right away. So, re-reviewing already reviewed work
   took more time that could be. It would be much more convenient if
   new changes from the last review were highlighted somehow in a way
   that you don't need to search for them manually.
5. __Hacktoberfest topics and labels__. For pull requests to be
   accepted during Hacktoberfest, you need to add the Hacktoberfest
   topic to your repository and add the `hacktoberfest-accepted`
   labels to PRs. This is extra work, and sometimes it's not even
   clear from the UI how to do this. Not to mention that it's possible
   to write those words with typos. It would be nice if you identify
   your Hacktoberfest participation in one click.
6. __GitHub Actions failure visibility__. Recently, the GitHub Actions
   UI was changed, and it doesn't use coloured labels for indicating
   failed steps. It takes more time to spot if tests passed or
   failed. In our course, we have some advanced tasks, but people are
   not required to solve them. So we add the `continue-on-error` flag
   for tests on advanced tasks. And, again, from the UI, it's not
   clear whether the step passed because it actually passed or because
   we skip errors. Using different indicators for different outcomes
   would help to spot such cases faster. Otherwise, we had to spend a
   lot of time jumping to the CI view and expanding steps to view the
   actual result.
7. __Hard to promote__. Because the course language is Haskell, it's
   more challenging to promote the course. The GitHub and
   Hacktoberfest UI don't have Haskell by default, and it is a bit
   more advanced to set up the search by Haskell. The trending
   repositories in Haskell also almost always contain the same popular
   3-4 repositories and no way for new projects to be highlighted
   there due to the GitHub algorithms. Learn4Haskell was almost never
   highlighted despite the relatively rapid growth of stars, forks and
   PRs.

### Haskell-specific

The Haskell language and its ecosystem also have several shortcomings
that made learning Haskell more difficult than it should
be. Specifically:

1. __Operating systems support__. Installing a working Haskell
   environment on Windows is still more difficult than it should
   be. macOS users also have weird linker errors sometimes when they
   try to build the project. And users of more exotic OSs like Arch,
   NixOS or Pop!_OS have troubles building the projects due to
   different linker issues as well.
2. __Excessive polymorphism in the standard library__. Polymorphic
   numeric constants scare people when they type the `:t 42` command
   and see the result. Also, types of innocently-looking functions
   like `length` and `concat` have the `Foldable` constraint instead
   of working with simple lists. People have hard times finding proper
   functions to solve their problems. Not to mention, that typeclasses
   and higher-kinded types are too advanced topics for beginners, and
   it would be much better if they can start coding without having
   "too many things they should ignore for now".
3. __GHC doesn't warn on common things by default__. You need to turn
   on the `-Wall` flag to enable the pattern matching checker. And if
   you add all common flags in the `.cabal` file, GHCi doesn't see
   them. So people often have common bugs in their solutions that GHC
   catches during compile-time if they simply run the `ghci`
   command. Adding such flags in the top of each module to make the
   `ghci` workflow smoother is awkward and less maintainable.
4. __Not enough extensions by default__. Similar to the previous
   point, some handy for education and explanation extensions
   (e.g. `InstanceSigs`) are not enabled by default. So you need to
   introduce people to the extensions too early, which could be easily
   avoided until later. However, the new
   [GHC20XX](https://github.com/ghc-proposals/ghc-proposals/pull/372)
   initiative seems to address this issue.
5. __Haskell formatters inconsistency__. We encountered a lot of
   issues due to people using formatters that change all the existing
   code. We use block-style comments to explain various topics. This
   helps avoid unnecessary noise and makes it easier to read, as we
   have really substantial comment blocks of explanations. But some
   people add Haskell formatters in their IDEs. As a consequence,
   formatters automatically change the style of comments to use line
   comments everywhere. This made reviewing such PRs more difficult
   and time-consuming, as we cannot see the code diff clearly.
6. __Let-in vs Where__. We've seen so many ways of people writing
   `let-in` expressions and `where`, it's not even funny... Haskell
   syntax rules are too flexible and allow too many syntactic
   constructions. At the same time, parser errors are not always
   helpful. Not to mention that scoping rules for `let-in` and
   `where`, especially in the presence of guards and multiple
   top-level pattern-matchings are far from obvious. Defining a
   variable shouldn't be that hard.
7. __Parentheses__. It's not so big of a problem, but in many places,
   it's not clear where you should use `()` or not. The rules around
   `()` are also inconsistent sometimes. So, people end up using a lot
   of `()` in their code.
8. __Spaces in function-arguments__. We do understand that lots of
   people have habits from their primary languages, where you use
   function calls with parentheses in the syntax like `fun(x)` or even
   `fun(x, y)`. Unfortunately, this is accepted in Haskell as well,
   though it could be misleading and should not be allowed.
9. __Spaces with operators__. Most of the people omitted spacing
   between operators, e.g. `1-2`. While this is valid in Haskell, it
   is better to avoid such writing. Mainly because some future changes
   in GHC (e.g. the RecordDot extension) could change the meaning of
   operators without spaces.
10. __Operator precedence__. Mixing different operators can be very
    frustrating sometimes. For instance, people sometimes are confused
    in the following situations, where similar expressions produce
    completely different results:

    ```haskell
    -1 `mod` 7
    ```

    and

    ```haskell
    mod (-1) 7
    ```

11. __GHC error messages__. GHC speaks for itself:

    ```
    • Couldn't match expected type 'b' with actual type 'Monster'
      'b' is a rigid type variable bound by
        the type signature for:
          fight :: forall b. Monster -> b -> String -> (Monster, b)
    ```

    People were not able to understand what is the problem in their
    code on their own by only looking at the compiler output. If the
    main benefit of a powerful compiler is to catch errors earlier,
    then the messages could be friendlier as well.

12. __Haskell syntax sometimes allows weird constructions__. For
    example, you can put `;` at the end of the line or even write the
    whole function (including type signature) on a single-line
    (apparently,
    [some code in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/b1a2c5e4508d61bc0bedc13c7240b6fbf674157e/compiler/GHC/Parser/PostProcess.hs#L171-193)
    uses this style). Turns out, you also can define typeclasses with
    curly brackets `{}` (similar to ordinary records). Did you also
    know that the `else` keyword in the `if-then-else` expression can
    be written from the start of the line?? Haskell is
    layout-sensitive language, but this mixing of layout-oriented and
    separator-oriented rules can be confusing.
13. __Inconsistent GHCi behaviour__. Sometimes, when people type the
    `:t 42` command in GHCi, they see the line `42 :: Num a => a`,
    sometimes `42 :: Num t => t`, and sometimes they see
    `42 :: Num p => p`. We use `doctest` to check the output of this
    command, but still for some people output is different from
    `doctest` expectations, they become frustrated, and we spend more
    time explaining the problem and solution.

## Feedback

Learn4Haskell is our first experience of conducting courses of this
particular shape. We haven't been working on anything similar before,
though Dmitrii has experience teaching Haskell in university and we
were mentoring people within our workplaces.

As we enjoyed both learning and prosecuting the project, we want to
improve the course more based on the feedback of both participants and
experienced Haskellers, as we proceed with the project. We did get
lots of different notes and opinions on the course from the former;
however, we didn't hear a lot from Haskellers.

Many people who tried our course were helping us by providing their
thoughts on the fly or filling our anonymous [feedback
form](https://docs.google.com/forms/d/e/1FAIpQLScBVhLxq5CgGnAfIGUE-fCoOUqeGkDY2HXzbT7KV2jjLOsmjQ/viewform)
afterwards. We've put together the results and want to share them with
everyone.

<hr>

Not every person finished all four chapters, though, the majority
among those who left feedback did so.

![Completed chapters](/images/posts/hacktoberfest2020/feedback_chapters.png)

If you are reading this blog post, but you did Learn4Haskell and
haven't submitted your feedback yet, feel free to do this afterwards!
We would appreciate your opinion.

<hr>

As for the course authors, it is crucial for us to keep the learning
interesting, so people would stay captivated and follow our course for
fun, and don't feel that it's a daily job or some mandatory university
course. It turns out that people really enjoy our content!

![Course interest](/images/posts/hacktoberfest2020/feedback_interest.png)

<hr>

We have been in Haskell for too long to be able to soberly assess the
level of difficulty of Learn4Haskell tasks from the beginners' point
of view. That's why we were interested in learners' opinions about our
course difficulty. It looks like we've managed to achieve quite the
right balance!

![Course difficulty](/images/posts/hacktoberfest2020/feedback_difficulty.png)

<hr>

We spend an enormous amount of our time on review as we hoped it is
key to the successful learning process. 100% of people who filled the
form seem to agree that they find reviews helpful, so no diagram this
time!

<hr>

Our goal for this course was to make it accessible to everyone. So it
was vital for us to know about people's backgrounds to see if we could
attract and help very diverse experienced folks. It turns out that
developers who tried our course have a diverse experience! They come
from languages like C#, C, C++, Java, Scala, Erlang, PHP, JavaScript,
Ruby, Python, Go, Rust, Common Lisp, R, Scheme, Prolog, Elm, Solidity,
as full-stack, backend or frontend engineers!

<hr>

In the end, a few highlights from people's feedback in the general
form:

::: {.cite-quote}
:::: {.cite-quote-content}
1. Progressive feedback on the solutions and unbelievable rapid reviews on the PR's.
2. Awesome mentors and fascinating exercises.

A job well done! Congratulations.
::::
:::

::: {.cite-quote}
:::: {.cite-quote-content}
It is a real bummer that there is no Chapter 5... ;)
::::
:::

::: {.cite-quote}
:::: {.cite-quote-content}
The course was structured well, TBH literally like a game :),
for a Haskell and FP newbie, it's well organized and If I compare my knowledge before and after, the delta is HUGE
::::
:::

::: {.cite-quote}
:::: {.cite-quote-content}
Exercises with dragons, castles, ... too distracting
Also probably a little too much dense...
::::
:::

::: {.cite-quote}
:::: {.cite-quote-content}
I thoroughly enjoyed the course material and exercises. I really
appreciate the maintainers taking time to review each and every line
of every single PR. That's really great. Usually we read some blog
post or book and may be try to implement some examples or
exercises. But, getting immediate feedback and suggestions for the
code implementation and getting a chance to work on improving it
just took the whole learning experience to the next level. Thanks a
lot for creating such wonderful tutorial. Brilliant idea.
::::
:::

<hr>

Thanks everyone who took their time to help us understand how we can
improve the course. We are already thinking about the further ideas on
how to make it better, and we will definitely want to continue with
that as it seems many people actually liked it!

## Lessons learned

While we were teaching Haskell and helping others, we ourselves gained
invaluable experience in areas of project management and starting such
initiatives.

### Motivation and Gratuitousness

This free Haskell course is the result of hard work. We've put a lot
of our experience and knowledge to make it accessible to as many
people as possible. The excitement was the main feeling we had when we
were launching the course. And imagine how hard it was to keep that
mood during the whole month with all the ups and downs of the
different participants' attitude.

It is hard to motivate ourselves to continue working on such a project
each day and doing the best we could each time, especially without any
external support. The only thing that kept us moving is people
enjoying the course, fascinated folks learning new things and sharing
their adventure steps. Every reaction even via simple emoji to our
comments mattered, every "thank you" gave us strength and filled with
the meaning all our days and nights when we stayed late, skipped
personal matters to sit and help people with learning.

Fun fact: to keep our engines working, we've created memos with all
the exciting, funny or just grateful comments people shared with
us. Just something to remind ourselves that at least somebody enjoys
our course, appreciates what we do, and it wasn't all for nothing :)
And we don't regret having it right now!

However, not all people were interacting with us (even though they
specifically asked for our review). Imagine spending half an hour
reviewing somebody's code very carefully and providing elaborate
comments with possible improvements and suggestions, and realising
that all that time you were speaking into the void. Or what is worth,
people completely disrespecting your opinions and knowledge and
attacking you instead. Also, some people just copy-pasted all our
official solutions, presented them as their own solutions and wasted
some of our time.

> It is not possible to create projects like this if you don't have
> enough motivation and support. It is not going to be a bed of roses,
> so be ready mentally before starting something similar.

### The Power of Diversity

The main "WOW" moment for us was the diversity of people's angles on
various topics.

We already had a chance to work with different people during our time
in open source, and that was incredible to see very novel and
unexpected ideas that were boosting the projects a lot.

But here, as we were looking at the solutions to the same issues, we
could feel the power of diversity even more! Many people were
implementing approaches we didn't even think about. And it was very
innovative! Sometimes it was more elegant, other times more
performant, or short, or expressive, or more readable, flexible or
extensive!

So, the lesson that we knew already as we ourselves are a team of
people with different backgrounds (both cultural and technical), but
rediscovered clearly again – __DIVERSITY is the key to successful
projects__!

To not be unfounded on our conclusions and to illustrate the diversity
of views, let's look at various implementations for some of our tasks.

<hr>

Even a simple function that returns the next integer was implemented
in many ways by different people:

```haskell
next x = x + 1
next x = 1 + x
next x = x+1
next = (+1)
next = (+ 1)
next = (1 +)
next = succ
...
```

> By the way, believe it or not, but the type signature to the `next`
> was implemented in that many ways as well!

<hr>

Or, to give an example of a bigger function, we were asking people to
implement the `subList` function that crops list elements starting
from a given index and ending under the given index, returning an
empty list on negative indexes.

Here is our official solution:

```haskell
subList :: Int -> Int -> [a] -> [a]
subList from to l
    | from < 0 || to < 0 || to < from = []
    | otherwise = take (to - from + 1) (drop from l)
```

But almost every person came out with their own version of `subList`,
sometimes even more efficient than ours! To highlight a few examples:

```haskell
subList :: Int -> Int -> [a] -> [a]
subList f t xs | f >= 0 && t >= f = take ((t + 1) - f) . drop f $ xs
               | otherwise = []
```

```haskell
subList :: Int -> Int -> [a] -> [a]
subList start end list = if start > end || start < 0 || end < 0
                            then []
                            else take (end - start + 1) (drop start list)
```

```haskell
subList :: Int -> Int -> [a] -> [a]
subList a b xs
  | a < 0 || b < 0 || b < a = []
  | otherwise = drop a (take (b+1) xs)
```

```haskell
subList :: Int -> Int -> [a] -> [a]
subList x y [] = []
subList x y z
             | (x < 0) || (y < 0) || (x > y) = []
             | (x == 0) && (y == 0) = take 1 z
             | (x == 0) = take y z
             | otherwise = take (y-x+1) (drop (x) z)
```

```haskell
subList :: Int -> Int -> [a] -> [a]
subList firstPoint secondPoint mainList = if firstPoint > secondPoint then [] else mainList!!firstPoint:subList (firstPoint + 1) secondPoint mainList
```
```haskell
subList i j l
    | i > length l || i < 0 || j < 0 = []
    | i <= j && j - 1 > length l = (l !! i) : subList (i + 1) (length l - 1) l
    | i <= j = (l !! i) : subList (i + 1) j l
    | otherwise = []

```

<hr>

All people read the same text, but they have different backgrounds,
views, and, as a consequence, come up with different solutions. You
can't pretend that "people should think the same way if they use the
same language" is possible. We are all different, and we should accept
our differences, embrace them and empower each other for their unique
qualities!

### Lonely journey

We love Haskell, and we are doing a lot of work to show the real power
of the language to people outside of the community as well.

Even though we believe that such a course is a tremendous boost to
Haskell popularity and accessibility, we can't say that this
initiative got a welcoming attitude or support from the Haskell
community itself. There may be some legitimate reasons why, however,
it is hard to say if there were strong and fundamental beliefs or just
personal dislike, as we didn't receive any feedback regarding it. The
only real reaction on the course "content" we got was that our logo
sucks and we are not good at design, which is maybe fair but not that
relevant to the course (and a bit rude, to be honest).

Though we got help from some Haskellers and participants through
(re)tweets, it was not very spreading. Our announcements on Reddit
even got downvoted a lot. The main problem for us was that we didn't
feel bolstered up.

The Learn4Haskell project per se is hard to sell to people because of

a) Haskell itself (not many people consider as something to invest their time in)
b) Hacktoberfest that was spoiled this year by some dishonest people and got a terrible reputation and anti-trend around it
c) many other reasons such as lack of promoting skills and complicated topics.

So we had to promote, maintain and improve the project mostly on our
own. Just the two of us in our free time, which was hard to deal with.

We want to highlight that we are grateful to everyone who helped us to
promote and showed their support! You rock! We do not belittle your
help; we just wanted to highlight the unexpected struggle of fighting
this battle almost alone that we need to deal with doing such projects
in Haskell as a community, which often feels like:

> Everyone in the community works on their own thing and collaboration
> is very rare.

### You can not make everyone happy

No matter what you do and how good you are at it, there will always be
people unsatisfied with your work. This is a hard pill to swallow, but
this is just the way it is. Even if you do something good, with good
intentions, for free.

Be ready to tune up some things, but also use your judgement to make
things you believe in better. Be true to yourself and to who you
are. For instance, we are not going to stop highlighting good moments
in the review even if not all people like it, especially if they don't
write explicitly what type of review they want.

Just to show a few aspects where different people have different
opinions:

  * Some people enjoyed the course, some found it not satisfying in
    content or the way the materials were given
  * Some said that some particular chapter was too difficult, others
    were particularly excited about the Chapter and noted that it was
    the best in the course
  * Some enjoyed Open-ended tasks, others wanted more strict
    descriptions
  * Some people liked the gaming fantasy theme, others found it
    distracting
  * Some people find our feedback motivating, other unnecessary and
    noisy
  * Some people thought that some of our advice makes code less
    readable, others were happy to see how more elegant and
    understandable it becomes (e.g. Pattern matching VS recursive go
    with the accumulators, removal of redundant parentheses vs keeping
    them)

As you see, there are too many opinions that could not just live
together. We tried to take the feedback in such opinionated topics
with a grain of salt. Our goals are the main orienteer for the
direction of the course, so we are happy to apply all suggestions that
align with it, but would be too careful to accept all the wishes of
all people.

## Conclusions

Our course proves that Haskell is not a difficult language. It turns
out that you can learn Haskell basics in several days, and start
coding in it very quickly. Haskell has a few distinguishing features
that make it easy for learning despite all language drawbacks (and
it's even possible to improve some aspects of the language to make it
more beginner-friendly without losing the power!). The critical point
of the teaching process of Haskell is to avoid the mindset of making
things look hard and complicating topics unnecessarily. Quite the
opposite, it is feasible to make difficult concepts look approachable!

At the same time, we want to mention that mentoring is key to
successful learning. It is one thing when you are learning a
completely new technology on your own, and it's entirely another thing
when there's somebody to guide you.

As the project creators, maintainers and mentors, we have mixed
feelings in the end.

On the one hand, a lot of people tried Haskell. Some of them were
scared to learn Haskell for years, but our course helped them to
overcome their fears. And for some people contributions to
Learn4Haskell were even their first pull requests ever! We welcomed
187 new people into the Haskell world, and we are eager to hear about
their successes more in the future!

However, leading such projects is hard work, and without any
appreciation and support, it's tough to find the motivation to
continue, and sometimes very depressing to keep doing it. We've spent
all our free time on review and support during the last month
sacrificing resting, reading, learning new stuff, watching TV series
and walking. We also have full-time jobs, so it was quite exhausting
to spend each free minute reviewing dozens of PRs. Approximately,
we've been spending about 40 hours per week only on Learn4Haskell,
besides other open-source stuff we need to do.

However, if you ask us, __was it worth it__, our answer is __YES__. We
have learned a lot, met wonderful people and got the feeling that even
just two of us can bring very significant improvements to this world!

<hr>

__Important Note:__

Even though Hacktoberfest is over, we are planning to review
everyone's solutions to [Learn4Haskell](@github(kowainik)) tasks as
long as we can. We are also going to improve the course after our
observations and feedback left by people who tried our course. So
don't despair if you didn't manage to learn Haskell during
Hacktoberfest. It is never too late to learn something new, and we are
here to help!

We hope our Learn4Haskell experience can inspire somebody, and who
knows, maybe next year it will be time to
__Learn4PutYourExcitingLanguageOrToolHere__!

## Thanks

Thanks to Taylor Fausak and Cameron Gera for mentioning Learn4Haskell
on [Haskell Weekly podcasts](https://haskellweekly.news/).

Thanks [cmdvtv](https://www.twitch.tv/cmdvtv) and
[jailandrade](https://www.twitch.tv/jailandrade) for streaming
Learn4Haskell and [sharing the content on the
YouTube](https://www.youtube.com/playlist?list=PLE-CSy3N6yEcQWBSOikTqeWeUv1GMw2lJ)
as well.

We want to say thank you to everyone involved in the course, all
participants, contributors, people who spread a word about the course
anywhere (special thanks to [kazup0n](@twitter) for providing
[Learn4Haskell instructions in Japanese](https://dev.classmethod.jp/articles/learn4haskell2020/)),
and those who supported us.

Several people supported us on [Ko-Fi](https://ko-fi.com/kowainik):
Gints Dremains, Donna, Joe Lucas, Abby S, @tfplug, Penny, cspan,
wookeny, Orca, Maxim, Naga Charan Meda, swamp_agr, Eric Moritz,
Nicolás Gargano and 5 other anonymous supporters on Ko-Fi ☕️ Thank you
all!
