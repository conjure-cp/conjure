# Conjure cask and formula for Homebrew

`conjure.rb` is a Homebrew cask for Conjure. It installs the appropriate MacOS binary distribution from github. This includes a full set of solvers except `z3`. Solver `z3` is not installed because it is a brew package and installing it could conflict with the brew version. *Note: running any of the solvers installed by this cask currently results in quarantine warnings since the solver binaries from github are not signed.*

`conjure-formula.rb` is a Homebrew formula for Conjure, but is still being developed. After installing some Python modules, this formula downloads a binary `stack` from the official Haskell web site and runs it to fetch the complete index of Haskell packages. It then builds Conjure from source: this currently requires some work-arounds to complete. Once built, Conjure is installed as well as an official binary distribution of Savile Row, but no additional solvers.


# How to install the cask

Run
```
brew install --cask conjure.rb
```
to install Conjure, Savile Row and a bunch of solvers to the Homebrew path.


# How to uninstall the cask

Run
```
brew uninstall --cask conjure
```
to remove Conjure, Savile Row, and the solvers it installed.


# How to install the formula

Run
```
brew install --formula formula/conjure.rb
```
to install Conjure and Savile Row. Note that this will not install any solvers, so you might want to run `brew install z3` to install a solver.

The formula currently uses the 2.4.0 release, which doesn't support the minimal path used by Homebrew for source builds.


# Todo

- [X] The ??? in the file for the intel release should be updated
- [X] Test on an intel machine
- [ ] Deal with JDK dependency properly in both cask and formula
- [ ] Work with z3 if already installed, otherwise install our own
- [ ] Ensure the formula build ends with Minion or another solver installed
- [ ] Formula fails to build 2.4.0, update to use a fixed release
- [ ] Release to homebrew?

