LIB_DIR = ./libcnet
CTEST_DIR = ./tests/stdlib

############################# TEST TARGETS ####################################
test: ccnet
	./runtests.sh
	@echo "SUCCESS"

############################# cnet top level ##################################

## cnet top level compiler that compiles and links a cnet source file into an executable
.PHONY: ccnet
ccnet: nobin cnet.native stdlib
	cd $(LIB_DIR) && make


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

####################### Std Lib ###########################
.PHONY: stdlib stdlib_tests

stdlib_tests:
	cd $(CTEST_DIR) && make all > /dev/null && make clean

stdlib:
	cd $(LIB_DIR) && make all > /dev/null

#############################  Other targets  #################################
.PHONY: clean
clean:
	opam config exec -- ocamlbuild -clean
	rm -rf final parser.ml parser.mli scanner.ml parser.output \
	scanner.ml scannertest scannertest.out *cmi *cmo \
	*.log *.diff *.out *.err *.ll *.s *.o parser.output \
	tests/integration/*.exe a.out a.out.dsYM/
	cd $(LIB_DIR) && make clean
	cd $(CTEST_DIR) && make clean

.PHONY: all
all: clean ccnet

# to make ocamlbuild happy
.PHONY: nobin
nobin:
	rm -f *.o *.a libcnet/*.o libcnet/*.a
