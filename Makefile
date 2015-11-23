
all:	tests

tests:	tests.deps tests.mlb
	./scripts/polybuild tests.mlb
# mlton tests.mlb
	./tests

tests.deps:	tests.mlb
	./scripts/dependencies $<

clean:
	rm -f tests

-include *.deps

