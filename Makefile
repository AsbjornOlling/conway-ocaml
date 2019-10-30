
run: all
	./conway.byte

all:
	ocamlbuild -use-ocamlfind conway.byte
