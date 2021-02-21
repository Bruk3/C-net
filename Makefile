
# The "opam exec --" part is for compatiblity with github CI actions
parser: 
	ocamlyacc parser.mly


test: clean parser runtests
	@echo "SUCCESS"

runtests: clean scannertest
	./runtests.sh

scannertest: scanner.cmo 
	ocamlc -o scannertest $^

scanner.cmo : scanner.ml
	ocamlc -c $^

scanner.ml : scanner.mll
	ocamllex $^

scannertest.out: scannertest test-scanner.tb 
	./scannertest < test-scanner.tb > scannertest.out


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
