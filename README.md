<p align="center">
  <a href="http://dev.stephendiehl.com/fun/">
    <img src="https://github.com/sdiehl/kaleidoscope/raw/master/img/dragon.png"/>
  </a>
</p>

<p align="center">
  <em>A short guide to building a tiny programming language in Haskell with LLVM.</em>
</p>

<p align="center">
  <a href="https://twitter.com/smdiehl">Stephen Diehl</a>
</p>

Haskell LLVM Tutorial
=====================

[![Build Status](https://travis-ci.org/sdiehl/kaleidoscope.svg)](https://travis-ci.org/sdiehl/kaleidoscope)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/kaleidoscope/blob/master/LICENSE-MIT)

Read Online:

* [**HTML**](http://www.stephendiehl.com/llvm)
* [**PDF**](http://www.stephendiehl.com/llvm/tutorial.pdf)
* [**Source Code**](https://github.com/llvm-hs/llvm-hs-kaleidoscope)
* [**Condensed Code**](https://github.com/llvm-hs/llvm-hs-kaleidoscope)


Setup
-----

You will need GHC 7.8 or newer as well as LLVM 4.0. For information on installing LLVM 4.0 (not 3.9 or earlier)
on your platform of choice, take a look at the
[instructions posted by the llvm-hs maintainers](https://github.com/llvm-hs/llvm-hs/blob/llvm-4/README.md#installing-llvm).

With Haskell and LLVM in place, you can use either Stack or Cabal to install the necessary Haskell
bindings and compile the source code from each chapter.

### Building with Stack (Recommended)

```bash
$ stack build
```

You can then run the source code from each chapter (starting with chapter 2) as follows:

```bash
$ stack exec chapter2
```

### Building with Cabal

Ensure that ``llvm-config`` is on your ``$PATH``, then run:

```bash
$ cabal sandbox init
$ cabal configure
$ cabal install --only-dependencies
```

Then to run the source code from each chapter (e.g. chapter 2):

```bash
$ cabal run chapter2
```

### Building with make

The source code for the example compiler of each chapter is included in the ``/src`` folder. With the dependencies
installed globally, these can be built using the Makefile at the root level:

```bash
$ make chapter2
$ make chapter6
```

A smaller version of the code without the parser frontend can be found in the
[llvm-tutorial-standalone](https://github.com/sdiehl/llvm-tutorial-standalone)
repository. The LLVM code generation technique is identical.

Editing
-------

This is an open source project, patches and corrections always welcome.

To generate the HTML page:

```bash
$ make tutorial.html
```

A standalone PDF can also be generated with:

```bash
$ make tutorial.pdf
```

License
-------

Text is adapted from the LLVM tutorial and is subsequently licensed under the
LLVM license.

The Haskell source files are released under the MIT license. Copyright (c)
2013-2016, Stephen Diehl
