# The Design of the Cake Build Libary

Cake is a build library in Haskell based on the ideas of Neil
Mitchell's *Shake* library.


## The Problem

One difficulty of (GNU) Make is that dependencies are defined
statically.  In many cases, however, it is be more convenient to
detect dependencies dynamically.  For example, when compiling a
Haskell file, we first need to read the file to find its dependencies.
Expressing this in Make is awkward.  The usual approach is to use a
separate preprocessing pass to find all dependencies which limits
concurrency in the build process.


## Shake's Idea

The basic idea of Shake is to have rules that only specify which
targets it generates.  Each rule has an associated action which can
request dependencies using the `need` primitive.  Shake logs the need
calls and its answers for each target, called the *history*.  This is
used to support incremental rebuilds.  If we want to rebuild a file,
we first check if there is a history for it, and if so check the
history for targets that need to be rebuilt.  If none of the targets
are out of date, nothing needs to be rebuilt.

Note that execution of build actions and dependency tracking is
interleaved.  Together with the availability of full Haskell in our
build language this makes for a very powerful build language.


## Requirements

  - maintainable, open source implementation

  - maximise potential concurrency

  - limit concurrency actual concurrency as requested by user

  - allow linting (rule sanity checks)

  - avoid deadlocks (or detect them if caused by rules)

  - non-file targets in a (preferably) Haskell98/2010 way


## Implementation

To keep the implementation as simple as possible we try to defer to
GHC's concurrent runtime wherever possible.

  - The `Database` contains the status of each file.

TODO...