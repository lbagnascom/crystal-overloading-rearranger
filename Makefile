
repl:
	ghcid --warnings --target=crystal-parser-test --restart=crystal-parser.cabal --test main

clean:
	rm -rf samples/default-values
	rm -rf samples/top-type
