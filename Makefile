all:
	ocamllex -q words.mll
	ocamlopt words.ml texpar.ml -o texpar

clean:
	rm -f *.o *.cmo *.cmx *.cmi words.ml
