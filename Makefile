
SCRIPTS	:= ext/sml-buildscripts

BUILDER	:= ${SCRIPTS}/polybuild

all:	ext convert example test/tests

convert:	convert.mlb d/convert.deps 
	${BUILDER} $<

example:	example.mlb d/example.deps
	${BUILDER} $<

test/tests:	test/tests.mlb d/tests.deps
	${BUILDER} $<
	$@

src/prerequisites.mlb:	ext

.PHONY:	ext
ext:	ext/sml-buildscripts/mlb-dependencies

ext/sml-buildscripts/mlb-dependencies:
	./vext install

MLBS	:= convert.mlb example.mlb test/tests.mlb $(wildcard src/*.mlb)

d/convert.deps:	convert.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

d/example.deps:	example.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

d/tests.deps:	test/tests.mlb $(MLBS)
	${SCRIPTS}/mlb-dependencies $< > $@

clean:
	rm -f convert example test/tests d/*

coverage:	ext
	${SCRIPTS}/mlb-coverage test/tests.mlb

.PHONY:	doc
doc:	ext
	mkdir -p doc
	ext/sml-buildscripts/mlb-expand src/sml-ttl.mlb | grep -v '^ext' > .docfiles
	smldoc --nowarn -d doc -a .docfiles

MLTON_ARGS	:= -runtime 'copy-generational-ratio 10.0' -runtime 'ram-slop 0.8'

release:	ext
	mlton $(MLTON_ARGS) convert.mlb
	mlton $(MLTON_ARGS) example.mlb
	mlton $(MLTON_ARGS) test/tests.mlb
	test/tests

-include d/*.deps

