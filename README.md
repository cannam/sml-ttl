
SML-TTL
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
build time using the Vext tool included.

To build and run tests using Poly/ML:

$ make

To build and run tests using MLton (slower compiling, faster runtime):

$ make release


Origin
------

Written by Chris Cannam, cannam@all-day-breakfast.com.  This code has
something in common with the Yertle library I wrote a couple of years
previously, which provides a very similar set of features in the Yeti
language. Both started as language-learning exercises.
