RESULT = typer
SOURCES = \
	parser.mly \
	lexer.mll \
	codemap.ml \
	errors.ml \
	ast.ml \
	type.ml \
	unif.ml \
	typer.ml \
	immtyper_test.ml \
	main.ml

include OCamlMakefile

JS_RESULT = $(RESULT).js

js: $(JS_RESULT)

$(JS_RESULT): byte-code
	js_of_ocaml $(RESULT)

cleanall: clean
	rm -f $(JS_RESULT)
