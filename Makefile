
# The "opam exec --" part is for compatiblity with github CI actions

test: test-scanner test-parser
	@echo "SUCCESS"

test-scanner: clean scannertest
	./runtests.sh

# TODO
%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

# parser: ast.ml
# 	ocamlyacc parser.mly

# scannertest: scanner.cmo 
# 	ocamlc -o scannertest $^

# scanner.cmo : scanner.ml
# 	ocamlc -c $^

# scanner.ml : scanner.mll parser
# 	ocamllex scanner.mll

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
	ocamlyacc parser.mly

ci-test: clean 
