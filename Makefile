
all:	load convert unit-tests

load:	d/load.deps load.mlb
	./scripts/polybuild load.mlb

convert:	d/convert.deps convert.mlb
	./scripts/polybuild convert.mlb

unit-tests:	d/unit-tests.deps unit-tests.mlb
	./scripts/polybuild unit-tests.mlb

MLBS	:= load.mlb convert.mlb unit-tests.mlb $(wildcard mlb/*.mlb)

d/load.deps:	load.mlb $(MLBS)
	./scripts/dependencies $< > $@

d/convert.deps:	convert.mlb $(MLBS)
	./scripts/dependencies $< > $@

d/unit-tests.deps:	unit-tests.mlb $(MLBS)
	./scripts/dependencies $< > $@

clean:
	rm -f load convert unit-tests

release:
	mlton load.mlb
	mlton convert.mlb
	mlton unit-tests.mlb
	./unit-tests

-include *.deps

