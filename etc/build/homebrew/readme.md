Set environment variable `EDITOR` to a preferred editor if the default `vi` is not a good choice, then run
```
brew create --cask --set-name conjure --no-fetch placeholder
```
and save the file. This puts a placeholder cask in the correct place. Now run
```
cp -p conjure.rb `brew edit --cask --print-path conjure`
```
to overwrite the placeholder with the contents of this cask.

Next run `brew install conjure` to copy Conjure, Savile Row and a bunch of solvers to your path.

Todo

- [X] The ??? in the file for the intel release should be updated
- [X] Test on an intel machine
- [ ] Deal with JDK dependency properly
- [ ] Work with z3 if already installed, otherwise install our own
- [ ] Release to homebrew?

