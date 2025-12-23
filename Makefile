.PHONY: all build test clean fmt lint install

all: build

build:
	dune build

test:
	dune runtest

clean:
	dune clean

fmt:
	dune fmt

lint:
	dune build @fmt

install:
	opam install . --deps-only --with-test --with-dev-setup -y

run:
	dune exec parser_combinator_demo

doc:
	dune build @doc

watch:
	dune build --watch
