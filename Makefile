
all:	tests load

tests:	tests.deps tests.mlb
	./scripts/polybuild tests.mlb
# mlton tests.mlb
	./tests

load:	load.deps load.mlb
	./scripts/polybuild load.mlb
# mlton load.mlb

tests.deps:	tests.mlb
	./scripts/dependencies $<

load.deps:	load.mlb
	./scripts/dependencies $<

clean:
	rm -f tests load

-include *.deps

