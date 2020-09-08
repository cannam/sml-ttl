
sml-ttl
=======

Simple RDF store and RDF/Turtle parser/serialiser written in Standard ML
------------------------------------------------------------------------

https://hg.sr.ht/~cannam/sml-ttl

This is an in-memory RDF triple store and RDF/Turtle parser/serialiser
written in Standard ML.

This library provides:

 * A 100% spec-compliant RDF/Turtle parser with fair performance

 * An RDF/Turtle serialiser

 * A simple single-graph datastore capable of doing reasonably quick
   indexed lookups (but not SPARQL queries)

The Turtle parser passes all current W3C spec tests at the time of
writing. The serialiser successfully serialises all of the test files
(plus some additional tests) such that they can be parsed in again.

An example program and a file conversion utility are included.


To build
--------

A Makefile is provided. Some additional SML libraries are pulled in at
build time using [Repoint](https://github.com/cannam/repoint).

To build and run tests using Poly/ML:

    $ make

To build and run tests using MLton (slower compiling, faster runtime):

    $ make release

You can build the bundled programs (e.g. `example.sml`, `convert.sml`)
with MLton using the supplied `.mlb` files, so long as Repoint has
pulled in the necessary dependencies (which the Makefile would do for
you):

    $ ./repoint install
    $ mlton example.mlb
    $ mlton convert.mlb

To extract the API documentation (must have `smldoc` installed,
e.g. from the fork at https://github.com/cannam/smldoc ):

    $ ./repoint install
    $ make doc

The code has been tested with a number of compilers on various
platforms:

* Linux and macOS CI build using Poly/ML, MLton, and SML/NJ: [![Build Status](https://travis-ci.org/cannam/sml-ttl.svg?branch=master)](https://travis-ci.org/cannam/sml-ttl)
* Windows CI build using SML/NJ: [![Build status](https://ci.appveyor.com/api/projects/status/bgelsc41d3k7i9ks?svg=true)](https://ci.appveyor.com/project/cannam/sml-ttl)


Origin
------

Written by Chris Cannam, cannam@all-day-breakfast.com. MIT licence,
see the file COPYING for details.

This code has something in common with the
[Yertle](https://hg.sr.ht/~cannam/yertle) library I wrote a couple
of years previously, which provided a very similar set of features in
the Yeti language. Both were initially language-learning exercises.

