
repl:
	ghcid --warnings --target=crystal-parser-test --restart=crystal-parser.cabal --test main

clean:
	rm -rf samples/default-values-functions
	rm -rf samples/default-values-methods
	rm -rf samples/top-type
	rm -rf samples/top-type-2-args
	rm -rf samples/top-type-2-args-placement
