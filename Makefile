
all:	tests load convert

tests:	tests.deps tests.mlb
	./scripts/polybuild tests.mlb
# mlton tests.mlb
	./tests

load:	load.deps load.mlb
	./scripts/polybuild load.mlb
# mlton load.mlb

convert:	convert.deps convert.mlb
	./scripts/polybuild convert.mlb
# mlton convert.mlb

tests.deps:	tests.mlb
	./scripts/dependencies $<

load.deps:	load.mlb
	./scripts/dependencies $<

convert.deps:	convert.mlb
	./scripts/dependencies $<

clean:
	rm -f tests load convert

-include *.deps

