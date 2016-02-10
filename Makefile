
all:	load convert unit-tests

load:	d/load.deps load.mlb
	./scripts/polybuild load.mlb

convert:	d/convert.deps convert.mlb
	./scripts/polybuild convert.mlb

unit-tests:	d/unit-tests.deps unit-tests.mlb
	./scripts/polybuild unit-tests.mlb
	./unit-tests

MLBS	:= load.mlb convert.mlb unit-tests.mlb $(wildcard mlb/*.mlb)

d/load.deps:	load.mlb $(MLBS)
	./scripts/dependencies $< > $@

d/convert.deps:	convert.mlb $(MLBS)
	./scripts/dependencies $< > $@

d/unit-tests.deps:	unit-tests.mlb $(MLBS)
	./scripts/dependencies $< > $@

clean:
	rm -f load convert unit-tests

coverage:
	./scripts/coverage.sh unit-tests.mlb

MLTON_ARGS	:= -runtime 'copy-generational-ratio 10.0' -runtime 'ram-slop 0.8'

release:
	mlton $(MLTON_ARGS) load.mlb
	mlton $(MLTON_ARGS) convert.mlb
	mlton $(MLTON_ARGS) unit-tests.mlb
	./unit-tests

-include d/*.deps

