
# The "opam exec --" part is for compatiblity with github CI actions
parser: 
	opam exec -- ocamlyacc cnet.mly

test: clean parser
	@echo "SUCCESS"


.PHONY: clean
clean:
	rm -f cnet.ml cnet.mli 

.PHONY: all
all: clean parser