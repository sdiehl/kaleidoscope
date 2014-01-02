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

License
-------

LLVM + MIT License
