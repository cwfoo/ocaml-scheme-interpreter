.PHONY: default
default: _scheme

_scheme: lexer.cmo parser.cmo sexpr.cmo env.cmo eval.cmo primitives.cmo main.ml
	ocamlc $^ -o $@

lexer_test: lexer.cmo parser.cmo lexer_test.ml
	ocamlc $^ -o $@

parser_test: lexer.cmo parser.cmo parser_test.ml
	ocamlc $^ -o $@

%.cmo: %.mli %.ml
	ocamlc -c $^

lexer.cmo: lexer.ml
	ocamlc -c $^

lexer.ml: parser.cmo lexer.mll
	ocamllex lexer.mll

parser.cmo: sexpr.cmo parser.ml
	ocamlc -c sexpr.cmo parser.mli parser.ml

parser.ml: parser.mly
	ocamlyacc $<

.PHONY: test
test: _scheme
	@echo "Scheme expressions."
	./scheme src-scheme/test-expressions.scm
	@echo "Primitives implemented in OCaml."
	./scheme src-scheme/test-primitives-ocaml.scm
	@echo "Primitives implemented in Scheme."
	./scheme src-scheme/test-primitives.scm

.PHONY: clean
clean:
	rm -f *\.cmo \.cmi \
		lexer.ml lexer.mli \
		parser.ml parser.mli \
		lexer_test parser_test \
		_scheme
