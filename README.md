Haskell Kaleidoscope Tutorial http://www.stephendiehl.com/llvm
===============================================================

This is an open source project, patches and corrections always welcome.

Editing
-------

Install the dependencies:

```bash
$ cabal sandbox init

If you just want to compile the examaples configure with the following. Ensure that ``llvm-config`` is on your
``$PATH``.

```bash
$ cabal configure
```

If you want to build the tutorial text locally configure with the following. This will install pandoc from
source which will take a while.

```bash
$ cabal configure --flags=tutorial
```

```bash
$ cabal install --only-dependencies
```

To generate the HTML page:

```bash
$ make tutorial.html
```

A standalone PDF can also be generated with:

```bash
$ make tutorial.pdf
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

License
-------

LLVM + MIT License
