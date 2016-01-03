Haskell LLVM Tutorial
=====================

[![Build Status](https://travis-ci.org/sdiehl/kaleidoscope.svg)](https://travis-ci.org/sdiehl/kaleidoscope)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/kaleidoscope/blob/master/LICENSE-MIT)

Read Online:

* [**HTML**](http://www.stephendiehl.com/llvm)
* [**Condensed Code**](https://github.com/sdiehl/llvm-tutorial-standalone)

Building with Stack ( Recommended )
-------

Using the LLVM toolchain requires several system libraries:

```bash
$ apt-get install llvm-3.5
$ apt-get install libedit-dev
```

The resulting page and chapter samples can be built using the given Makefile.

```bash
$ stack build
$ stack ghc preprocessor.hs -- -o preprocessor 
$ stack exec make
```

Building with Cabal
-------

```bash
$ apt-get install llvm-3.5
$ apt-get install libedit-dev
```

Install the dependencies:

```bash
$ cabal sandbox init
```

If you just want to compile the examples then configure with the following command. Ensure that
``llvm-config`` is on your ``$PATH``.

```bash
$ cabal configure
```

If you want to build the tutorial text locally configure with the following. This will install pandoc from
source which will take a while.

```bash
$ cabal configure --flags=tutorial
```

Then install the dependencies:

```bash
$ cabal install --only-dependencies
```

Building with Nix
-------

If you are currently using Nix package manager or NixOS, is to install all
dependencies, both Haskell and system libraries, is to use the ``default.nix``
configuration provided. From the source directory simply run:

```bash
$ nix-shell
```

This will install binary packages for all dependencies including ``ghc``, ``llvm``, ``haskeline`` and
``llvm-general`` in a self-contained environment that is very likely to work out of the box.


Source Code
-----------

The source code for the example compiler of each chapter is included in
the ``/src`` folder. With the dependencies installed these can be built
using the Makefile at the root level or with cabal.

```bash
$ cabal run chapter2
$ cabal run chapter6
```

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
