# The "opam exec --" part is for compatiblity with github CI actions
############################# TEST TARGETS ####################################
test: test-scanner test-parser
	@echo "SUCCESS"

test-scanner: cnet.native
	./runtests.sh

test-parser:
	ocamlyacc -v parser.mly
	rm parser.ml parser.mli



############################# cnet top level ##################################

## cnet top level - Currently supports two flags
##  -a (ast pretty printing), -t (token pretty printing)
cnet.native: cnet
	opam config exec -- \
		ocamlbuild -use-ocamlfind cnet.native


####################### Dependencies for ocamlbuild ###########################

.PHONY: scanner parser ast

cnet: cnet.ml scanner parser ast scanner_pp

scanner: scanner.mll parser.mly
scanner_pp: scanner_pp.ml
parser: parser.mly ast.ml
ast: ast.ml


#############################  Other targets  #################################
.PHONY: clean
clean:
	opam config exec -- ocamlbuild -clean
	rm -f final parser.ml parser.mli scanner.ml parser.output \
	scanner.ml scannertest scannertest.out *cmi *cmo \
	*.log *.diff *.out *.err

.PHONY: all
all: clean parser


##################################################################
###### Targets below are for the github action workflows #########


ci-parser:
	opam exec -- ocamlyacc parser.mly

ci-test: ci-parser clean test-scanner

