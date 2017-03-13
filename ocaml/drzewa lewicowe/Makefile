#test: leftist clean
#	leftist
testy: leftist
	./leftist

leftist: leftist.cmo testy.cmo
	ocamlc -o $@ $^

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm *.cmi *.cmo
