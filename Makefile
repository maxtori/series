all: build

js:
	@dune build --profile release src/js
	@cp -f _build/default/src/js/ui.bc.js www/series.js
build:
	@dune build --profile release
	@mkdir -p _bin
	@cp -f _build/default/src/unix/main.exe _bin/series
	@cp -f _build/default/src/js/ui.bc.js www/series.js
clean:
	@dune clean
