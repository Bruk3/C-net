
# The "opam exec --" part is for compatiblity with github CI actions

test: test-scanner test-parser
	@echo "SUCCESS"

test-scanner: clean scannertest
	./runtests.sh

# TODO
test-parser: 

parser: 
	ocamlyacc parser.mly

scannertest: scanner.cmo 
	ocamlc -o scannertest $^

scanner.cmo : scanner.ml
	ocamlc -c $^

scanner.ml : scanner.mll
	ocamllex $^

.PHONY: clean
clean:
	rm -f parser.ml parser.mli scanner.ml \
	scannertest scannertest.out *cmi *cmo \
	*.log *.diff 

.PHONY: all
all: clean parser


##################################################################
###### Targets below are for the github action workflows #########


ci-parser:
	opam exec -- ocamlyacc parser.mly

ci-test: clean ci-parser

