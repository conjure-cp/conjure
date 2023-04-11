# Conjure cask and formula for Homebrew

`conjure.rb` is a Homebrew cask for Conjure. It installs the appropriate MacOS binary distribution from github. This includes a full set of solvers except `z3`. Solver `z3` is not installed because it is a brew package and installing it could conflict with the brew version. *Note: installing this cask currently results in quarantine warnings since these binaries from github are not signed.*

`conjure-formula.rb` is a Homebrew formula for Conjure. This process first runs a binary `stack` downloaded from the official Haskell web site and then downloads a large index of Haskell packages. It then builds Conjure from source and installs it, as well as an official binary distribution of Savile Row.


# How to install the cask

Set environment variable `EDITOR` to a preferred editor if the default `vi` is not a good choice, then run
```
brew create --cask --set-name conjure --no-fetch placeholder
```
and save the file. This puts a placeholder cask in the correct place. Now run
```
cp -p conjure.rb `brew edit --cask --print-path conjure`
```
to overwrite the placeholder with the contents of this cask.

Next run `brew install conjure` to copy Conjure, Savile Row and a bunch of solvers to the Homebrew path.


# How to install the formula

Set environment variable `EDITOR` to a preferred editor if the default `vi` is not a good choice, then run
```
brew create --formula --set-name conjure --no-fetch placeholder
```
and save the file. This puts a placeholder cask in the correct place. Now run
```
cp -p conjure-formula.rb `brew edit --formula --print-path conjure`
```
to overwrite the placeholder with the contents of this formula.

Next run `brew install conjure` to install Conjure and Savile Row to the Homebrew cellar. Note that this will not install any solvers, so you might want to run `brew install z3` to install a suitable solver.


# Todo

- [X] The ??? in the file for the intel release should be updated
- [X] Test on an intel machine
- [ ] Deal with JDK dependency properly
- [ ] Work with z3 if already installed, otherwise install our own
- [ ] Ensure the formula build ends with Minion or another solver installed
- [ ] Release to homebrew?

