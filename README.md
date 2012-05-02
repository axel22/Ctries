# Ctrie - A Lock-Free Concurrent Hash Array Mapped Trie

A concurrent hash-trie or Ctrie is a concurrent thread-safe lock-free implementation of a hash array mapped trie. It is used to implement the concurrent map abstraction. It has particularly scalable concurrent insert and remove operations and is memory-efficient. It supports O(1), atomic, lock-free snapshots which are used to implement linearizable lock-free size, iterator and clear operations. The cost of evaluating the (lazy) snapshot is distributed across subsequent updates, thus making snapshot evaluation horizontally scalable.

The implementation is written in Scala (http://www.scala-lang.org/), a statically compiled language for the JVM. It is fully compliant with the collection package from the Scala standard library.

As the Ctrie is a part of the Scala standard library since the version 2.10, clients using versions 2.9.x can use this implementation.

More info about Ctries:

- http://infoscience.epfl.ch/record/166908/files/ctries-techreport.pdf - this is a nice introduction to Ctries, along with a correctness proof
- http://lamp.epfl.ch/~prokopec/ctries-snapshot.pdf - a more up-to-date writeup (more coherent with the current version of the code) which describes the snapshot operation


## How to run

__1) Requirements__

- JDK1.6 or higher
- SBT (simple build tool) - see http://code.google.com/p/simple-build-tool/wiki/Setup

__2) Run SBT__

Once the `sbt` command is run in the root directory of the project, it will download the appropriate Scala library and compiler from Maven. It will start the SBT interactive shell. Generally, to run SBT tasks, you have to start the SBT shell.

__3) Update dependencies__

Within the SBT shell type the command:

    > update

and hit ENTER. This will resolve all the library dependencies from Maven (e.g. the ScalaTest library).

__4) Compile__

Within the SBT shell:

    > compile

This will compile the project. After this, you can run tests using the `test` command or run benchmarks using the `bench` command.


### Packages

The `ctries2` package contains an up-to-date version of the Ctrie data structure.

The `ctries` package contains a previous version of the Ctrie data structure. The difference is that the previous version uses one I-node per a stored key-value pair, resulting in an additional indirection and increased memory usage. This previous version is useful to compare performance and memory usage against the preferred version in the `ctries2` package.


### Branches

The `master` branch contains an up-to-date version of the Ctrie data structure with snapshot support implemented.

The `no-snapshots` branch contains the version of the Ctrie data structure without snapshot support. This branch is useful to compare snapshot support overhead (in my experiments 10-20% slowdown on a 1M element dataset).


### Tests

Tests are run using the `test` command in the SBT shell. They can also be run selectively like this:

    > test-only ctries2.LNodeSpec

which runs the tests associated with L-nodes.


### Benchmarks

Benchmarks are located in the `src/bench/scala` directory. They are run by using the `bench` command in the SBT shell, which will start a new JVM instance, do the warmup and run the selected snippet a number of times. Generally, you will have to inspect what parameters the specific benchmark expects in order to run it. For example, to run the insertion benchmark for 500k elements, using 4 threads and repeat this test 6 times, you have to write:

    > bench -Dsz=500000 -Dpar=4 ctries.MultiInsertCtrie2 6 1

This specific benchmark will evenly distribute the work of inserting `sz` elements into an empty Ctrie between `par` threads.

It's also possible to run benchmarks in batches to produce a range of results using the `bench-batch` task in the SBT shell. This will run a Perl script to gather the data and convert them to Latex tikz format used for figures.

All benchmarks presented in the paper are available within this project.
