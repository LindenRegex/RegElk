all:main tests fuzzer stats benchmark matcher

FILES = benchmark_vectors.ml timer.ml oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml tojs.ml toexp.ml cdn.ml anchors.ml tests.ml charclasses.ml flags.ml

PARSER_SRC = parser_src/regex_lexer.mll parser_src/regex_parser.mly
PARSER_FILES = parser/regex_lexer.ml parser/regex_parser.ml

parser/regex_lexer.ml: parser_src/regex_lexer.mll
	ocamllex -o parser/regex_lexer.ml parser_src/regex_lexer.mll

parser/regex_parser.ml: parser_src/regex_parser.mly
	menhir parser_src/regex_parser.mly 2>/dev/null
	mv parser_src/regex_parser.ml parser_src/regex_parser.mli parser

parser_clean:
	rm -f parser/*

%.native: %.ml $(FILES) $(PARSER_FILES) $(PARSER_SRC)
	ocamlbuild -I parser -package unix -package ocaml_intrinsics -package core_bench -package core -package core_unix.command_unix -package yojson $@


main: main.native
tests: tests.native
fuzzer: fuzzer.native
stats: stats.native
linearbaseline: linearbaseline.native
matcher: matcher.native
benchmark: matcher linearbaseline benchmark.native

clean: parser_clean
	rm -f -R _build
	rm -f *.native
	rm -f *.data

clean_data:
	rm -f *.csv
	rm -f *.jpg
