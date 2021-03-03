
# The "opam exec --" part is for compatiblity with github CI actions

################ TEST TARGETS ######################
test: test-scanner test-parser
	@echo "SUCCESS"

cnet.native: 
	opam config exec -- \
		ocamlbuild -use-ocamlfind cnet.native

test-scanner: clean scannertest
	./runtests.sh

scannertest: scanner.cmo 
	ocamlc -o scannertest $^

# TODO
test-parser: 

############### END TEST TARGETS ###################

cnet: parser.cmo scanner.cmo
	ocamlc -o final $^


%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

scanner.ml: scanner.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc $^

# Dependencies for opening modules
scanner.cmo: scanner.ml parser.cmi
	ocamlc -c $<

parser.cmo: parser.ml parser.cmi ast.cmo
	ocamlc -c $<

parser.cmi: parser.mli ast.cmo

.PHONY: clean
clean:
	rm -f final parser.ml parser.mli scanner.ml parser.output \
	scanner.ml scannertest scannertest.out *cmi *cmo \
	*.log *.diff 

.PHONY: all
all: clean parser


##################################################################
###### Targets below are for the github action workflows #########


ci-parser:
	opam exec -- ocamlyacc parser.mly

ci-test: clean ci-parser

