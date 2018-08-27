
# v2.2.0 (2018-08-27)

- Bug fix: Avoid generating names that are used in the input file.
- Improvements to the refinement of partitions. Do not generate redundant structural constraints.
- Many more (I promise to write them soon!)

For now, check [this page](https://github.com/conjure-cp/conjure/compare/v2.1.0...v2.2.0) which lists all changes since the previous release.

# v2.1.0 (2017-08-24)

This release contains bug fixes, general improvements, and much more comprehensive documentation!

[Here](https://github.com/conjure-cp/conjure/compare/v2.0.0...v2.1.0) is a list of all changes since the previous release.

Some noteworthy changes are the following.

- Bug fix: Handling of aliases (letting statements) (#352, #368)
- Bug fix: Pretty-printing of right-associative operators (#354)
- Bug fix: Refinement of the powerSet operator (#370)
- Improvements to the expression parser. It gives slightly better error messages now, although there is still room for improvement.
- Added a command line argument (--responses). This allows scripting Conjure instead of making modelling choices interactively.
- Added some more "visualisations" to be used in the solution files.
- Improved the MSetExplicitWithRepitition to use fewer decision variables.
- Added some simple implied constraints to do with partition domains.
- Better refinement of allDiff constraints that contain a list comprehension in them. This was tricky since the number of elements in the list may depend on the values of decision variables.
- Improved documentation, thanks to @ott2. The [Essence](https://conjure.readthedocs.io/en/v2.1.0/essence.html) section and the [Demonstrations](https://conjure.readthedocs.io/en/v2.1.0/demonstrations.html) section are particularly helpful to newcomers. We aim to improve the documentation further, but this has been a huge improvement.


# v2.0.0 (2017-02-03)

We made various versions of Conjure available in the past, but this is the first release of it on GitHub. Yay!

We intend to use GitHub to release future versions too.

