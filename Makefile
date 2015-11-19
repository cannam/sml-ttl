
all:	test

test:	test.mlb
	./scripts/polybuild test.mlb
	mlton test.mlb
	./test

test.deps:	test.mlb
	./scripts/dependencies $<

clean:
	rm -f test

-include *.deps

