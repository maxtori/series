all: build

build:
	@dune build src/common
	@dune build src/unix/main.exe
	@cp -uf _build/default/src/unix/main.exe bin/series.exe
	@dune build --profile release src/js/ui.bc.js
	@cp -uf _build/default/src/js/ui.bc.js www/ui.js

clean:
	@dune clean
