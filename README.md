# Schemer

A simple compiler of a subset of Scheme language.

## Introduction

In this project, we will write a compiler for a subset of Scheme that produces assembly language code for the 64-bit x86-64 architecture.

Actually, the project is comprised of 15 labs, with each of them having its own testcases.

Each lab is based on the precedented one, so make sure all testcases of previous labs are passed when you are ready to a new lab.

The subset of Scheme will grow larger as the project progresses. The compiler will also become more sophisticated in other ways, primarily to generate more efficient code.

<font color="red">
Don't be afraid of x86-64 assembly language.

Indeed, it's really confusing when you first use it, but you will be skilled with it in just 2 days since all we need is just a very tiny part of it.
</font>

## Getting started

Your main program should be in a new directory named `src` under the root directory, and its name should be `schemer.scm`.

It's highly recommended to break the main program into several independent parts. But how to deal with the problem is up to you, while it's also permittable to write the compiler in just one file.

Once you are ready to get started, you need to:

1. Move into the repo's root, running `git fetch` to retrieve the most recent version of the project.
2. Each part is on a new branch. For example, if you are ready to begin with `a1`, type in `git merge origin/a1` in your terminal. It's important to confirm that you are now in your `master` or `main` branch.
3. If merging works well, there will be a new `a*` sub-directory in your repo. All documents about this lab are under the sub-directory.

## Testing advice

We have provided a whole toolchain. To test your compiler, you just need to:

1. Type in `make a*` under your root directory. For example, if you want to test the `a1`, just run `make a1`.
2. Now, there will be a `test.scm` under the root. Load this into Chez Scheme, and type
    - `(test-all)` to run all of the valid tests,
    - `(test-one <test-name>)` to run an individual test.