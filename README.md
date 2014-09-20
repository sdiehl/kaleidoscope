Haskell Kaleidoscope Tutorial http://www.stephendiehl.com/llvm
===============================================================

This is an open source project, patches and corrections always welcome.

Installing with Nix
-------

The easiest way (if you are currently using Nix package manager or NixOS) is to install all dependencies, both
Haskell and system libraries, is to use the ``default.nix`` configuration provided. From the source directory
simply run:

```bash
$ nix-shell
```

This will install binary packages for all dependencies including ``ghc``, ``llvm``, ``haskeline`` and
``llvm-general`` in a self-contained environment that is very likely to work out of the box.

Installing with Cabal
-------

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

Source Code
-----------

The source code for the example compiler of each chapter is included in
the ``/src`` folder. With the dependencies installed these can be built
using the Makefile at the root level.

```bash
$ make chapter1
$ make chapter6
```

Editing
-------

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

LLVM + MIT License
