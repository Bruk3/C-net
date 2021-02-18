
# The "opam exec --" part is for compatiblity with github CI actions
parser: 
	opam exec -- ocamlyacc parser.mly

test: clean parser
	@echo "SUCCESS"


.PHONY: clean
clean:
	rm -f parser.ml parser.mli 

.PHONY: all
all: clean parser
