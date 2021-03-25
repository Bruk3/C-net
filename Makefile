# The "opam exec --" part is for compatiblity with github CI actions
############################# TEST TARGETS ####################################
test: test-scanner test-parser
	@echo "SUCCESS"

test-scanner: ccnet
	./runtests.sh

test-parser:
	ocamlyacc -v parser.mly
	rm parser.ml parser.mli



############################# cnet top level ##################################

## cnet top level compiler that compiles and links a cnet source file into an executable
.PHONY: cnet
ccnet: cnet.native

# cnet compiler that translates .cnet -> .ll
# supports the following flags:
# -t : scanner pretty printing
# -a : ast pretty printing
# -s : sast pretty printing
# -l : llvm IR pretty printing (no llvm checking)
# -c : compiler (with llvm checking)
cnet.native: cnet
	opam config exec -- \
		ocamlbuild -use-ocamlfind cnet.native

####################### Dependencies for ocamlbuild ###########################

.PHONY: scanner parser ast

cnet: cnet.ml scanner parser ast scanner_pp

scanner: scanner.mll parser.mly utils.ml
scanner_pp: scanner_pp.ml
parser: parser.mly ast.ml
ast: ast.ml


#############################  Other targets  #################################
.PHONY: clean
clean:
	opam config exec -- ocamlbuild -clean
	rm -f final parser.ml parser.mli scanner.ml parser.output \
	scanner.ml scannertest scannertest.out *cmi *cmo \
	*.log *.diff *.out *.err *.ll *.s *.o parser.output \
	tests/integration/*.exe

.PHONY: all
all: clean cnet.native printbig.o


##################################################################
###### Targets below are for the github action workflows #########


ci-parser:
	opam exec -- ocamlyacc parser.mly

ci-test: ci-parser clean test-scanner

