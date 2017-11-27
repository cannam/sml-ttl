
sml-ttl
=======

Simple RDF store and RDF/Turtle parser/serialiser written in Standard ML
------------------------------------------------------------------------

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
build time using [Vext](https://github.com/cannam/vext).

To build and run tests using Poly/ML:

    $ make

To build and run tests using MLton (slower compiling, faster runtime):

    $ make release

You can build the bundled programs (e.g. `example.sml`, `convert.sml`)
with MLton using the supplied `.mlb` files, so long as Vext has pulled
in the necessary dependencies (which the Makefile would do for you):

    $ ./vext install
    $ mlton example.mlb
    $ mlton convert.mlb

To extract the API documentation (must have `smldoc` installed,
e.g. from the fork at https://github.com/cannam/smldoc):

    $ make doc


Origin
------

Written by Chris Cannam, cannam@all-day-breakfast.com. MIT licence,
see the file COPYING for details.

This code has something in common with the
[Yertle](https://bitbucket.org/cannam/yertle) library I wrote a couple
of years previously, which provided a very similar set of features in
the Yeti language. Both were initially language-learning exercises.

