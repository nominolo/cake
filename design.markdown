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


## Basic Principles

Here is a very simple usage example:

    import Development.Cake

    main = cake $ do
      "AB.txt" *> \fp -> do
         -- > $ cat A.txt B.txt > AB.txt
         let files = ["A.txt", "B.txt"]
         need files
         fs <- mapM (io . readFile) files
         io (writeFile (concat fs))

      want ["AB.txt"]

This file specifies one rule to build the file "AB.txt".  The rule's
body describes how to create "AB.txt" by concatenating files "A.txt"
and "B.txt".  The interesting part is the call to the function `need`
which requests that the following action will read these requested
files.  The call to `need` ensures that these files are made
up-to-date *before* continuing with the execution of the rule action.
If "A.txt" or "B.txt" are generated files themselves, they will be
generated now.

While executing a rule action, we record a history of calls to need.
We record the modification time of each requested file in the history
and eventually store the history of all files on disk.  In a future
invocation of `cake` any calls to `need` will first check the history
whether any dependencies have changed.

Note that it is up to the user to ensure the correctness of rule
outputs and inputs via `need`.  Most rules should not use need
directly and instead use a set of library functions such as `ls`,
`cp`, etc.  which can be audited once for all users.

The fact that `need` takes a list is no coincidence.  The implicit
conttract is that all arguments to a single `need` call can be built
in parallel.  In fact, it is one of the main goals of `Cake` to allow
building with as much paralellism as possible.

We use two monads:
 
 1. The `Cake` monad is the outer monad and allows adding rules and
    requesting targets via `want`.  It is executed using the `cake`
    function.

 2. The `Act` monad is used in the body of rules.  Most notably
    it allows `need` calls and any `IO` operations that need to
    be performed by an action.

Aside: There are some build tools which use tracing of system calls to
get accurate dependencies of a build command.  Note that `need` does
more than this.  Calling `need` first recursively makes sure that
all the requested dependencies are up to date and then records the
dependencies taken.


## Implementation

To keep the implementation as simple as possible we try to defer to
GHC's concurrent runtime wherever possible.

  - The `Database` contains the status of each file.

  - A file's status can be:
  
     - `Clean`: the file is known to be up to date

     - `Dirty`: we have a history for that file, but we don't
       know whether the file needs to be rebuilt.

     - `Building`: the file is currently being processed by
       some build thread.

     - Not in the database.  In that case we have to rebuild the file.

  - A call to `need` thus concurrently checks the status for each
    file and performs one of the possible actions:

     - If the file is clean, nothing needs to be done.

     - If the file is dirty, we need to check its history.  This in
       turn will require checking each of the file's dependencies and
       so on.  If the history check failes, the file needs to be
       rebuilt.

     - The file is building, in this case we have to wait for the
       build thread to finish.

  - Looking up a file's status and deciding what to do must be a
    single atomic action.

There are a few sublteties with the steps above.  A rule may build
multiple targets.  Whenever a file is not `Clean` or `Building` we
must look up the corresponding rule (there should be only one) and
lock all targets at once.  Otherwise, concurrently executing threads
may try to execute the same rule.

[TODO: exceptions story]

Because we want limited concurrency (number of CPUs + small constant
to account for I/O delay), we want to implement the above in a model
where a fixed number of workers is concurrently building all targets.

There are two obvious approaches to implement this:

 1. Maintain a global work queue and workers just repeatedly grab
    things from the work queue until everything is done.

    This may have problems conceptially where a worker becomes blocked
    on one task (e.g., it's waiting for a dependency to build).  In
    this case we want the worker to start working on something else.
    There are two options:

     a. The thread blocks, but no longer counts as a worker (by
        releasing a counting mutex).  Once the thread becomes
        unblocked it tries to become a worker again (by grabbing the
        counting mutex).

     b. We grab the current continuation and store it in the database.
        The worker is then free to to pick another task.  Unless an
        error occurs, some worker will eventually come back and pick
        up the continuation.

    Option (a) is simpler, because in Option (b) we would have to
    explicitly check that all dependencies are ready.  In Option (a)
    we just block on all our dependencies and automatically get woken
    up once the dependencies are ready.  In essence, the Haskell
    runtime system does the continuation management for us.

 2. Build all dependencies in parallel and use a library to limit the
    level of concurrency.  Combined with Option (a) above, this would
    mean we could hand off the work queue managment to the Haskell
    thread scheduler as well.

    A good library to control the level of concurrency is Max
    Bolingbroke's [parallel-io][1] package.

[1]: http://hackage.haskell.org/package/parallel-io

Cake currently uses Option 2 above.
