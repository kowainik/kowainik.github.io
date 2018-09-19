# kowainik.github.io

Kowainik web page: kowainik.github.io


## How to update this web page

If you want to change content of Kowainik web page you need to perform the following steps:

1. Make sure that you are on the `develop` branch
2. Create new branch from `develop`, implement desired changes and open pull request
3. The person who merges PR should deploy new web page content with the following command:
```
./scripts/deploy.sh "Some meaningful message"
```
## How to add blog post

If you want to add a new post you should create markdown file in the `posts-raw/` folder. The name of this file should contain the date of the post and some name. For example: `2018-11-05-kowainik-new-project.md`.

In the `.md` file you should add next info in the following format:

```
---
title: Some really meaningful title that will appear at the page
author: Your Name
tags: haskell, stack, cabal, build-tools, tutorial
---

DO NOT COPY TITLE HERE!
Here comes the body of the post itself

## Important rules!!!

* Use only `##` and upper for headers.
* Avoid special characters in the headers names (including `\``).
* Do not start any other lines with `#` (even in code examples).
* Tags should be one-worders.

...

```

Note, that tags should be one-worders for the moment.
