
# The "opam exec --" part is for compatiblity with github CI actions
parser: 
	ocamlyacc parser.mly

test: clean parser
	@echo "SUCCESS"

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
	rm -f parser.ml parser.mli scanner.ml scannertest scannertest.out *cmi *cmo

.PHONY: all
all: clean parser
