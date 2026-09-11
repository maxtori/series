all: build
build:
	@dune build --profile release
js:
	@dune build --profile release @install
clean:
	@dune clean
