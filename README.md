# dream
A study project for my free time: implementing a compiler in Haskell.

The original language used in my compilers course is called e2, here I'm trying to work with e2 as a basis and enhance it further,
but first of all I gotta port it to Haskell, the e2c is implemented in Java (yuck!).

## Usage
The project is compiled with cabal. Since it is not very complex yet, this doesn't require much explanation.
There are two targets/executables:
1. The actual dream compiler, used like gcc with various commandline arguments
```console
$ cabal run dreamc
```
arguments to dreamc go after `--`

example:
```console
$ cabal run dreamc -- test-programs/plot.dream -o plot --keep
```
2. The dream interpreter
```console
$ cabal run dreamci
```
