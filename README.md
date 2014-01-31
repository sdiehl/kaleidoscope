Haskell Kaleidoscope Tutorial http://www.stephendiehl.com/llvm
===============================================================

This is an open source project, patches and corrections always welcome.

Editing
-------

Install the dependencies:

```bash
$ cabal sandbox init
$ cabal configure
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
