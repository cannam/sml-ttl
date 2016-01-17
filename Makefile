
all:	load convert unit-tests

load:	load.deps load.mlb
	./scripts/polybuild load.mlb

convert:	convert.deps convert.mlb
	./scripts/polybuild convert.mlb

unit-tests:	unit-tests.deps unit-tests.mlb
	./scripts/polybuild unit-tests.mlb

load.deps:	load.mlb
	./scripts/dependencies $<

convert.deps:	convert.mlb
	./scripts/dependencies $<

unit-tests.deps:	unit-tests.mlb
	./scripts/dependencies $<

clean:
	rm -f load convert unit-tests

release:
	mlton load.mlb
	mlton convert.mlb
	mlton unit-tests.mlb
	./unit-tests

-include *.deps

