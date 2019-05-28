# kowainik.github.io

Kowainik web page:

* [kowainik.github.io](https://kowainik.github.io/)

## How to update this web page

If you want to change the content of the Kowainik web page you need to perform
the following steps:

1. Make sure that you are on the `develop` branch
2. Create new branch from `develop`, implement desired changes and open a pull request
3. The person who merges the PR should build the project and deploy the new web
   page content with the following command:

```
./scripts/deploy.sh "Some meaningful message"
```

## How to add a blog post

If you want to add a new post you should create a markdown file in the `posts/`
folder. The name of this file should contain the date of the post and some
meaningful name. For example: `2018-11-05-kowainik-new-project.md`.

In the `.md` file you should add next info in the following format:

```
---
title: Some really meaningful title that will appear at the page
author: Your Name
tags: haskell, stack, cabal, build-tools, tutorial
description: Some short description
---

DO NOT COPY TITLE HERE!
Here comes the body of the post itself

## Important rules!!!

* Use only `##` and upper for headers.
* Avoid special characters in the headers names (including `\``).
* Tags should be one-worders.

...

```
