test: build
	./build

build: arytmetyka.cmo arytest.cmo
	ocamlc -o $@ $^

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm *.cmi *.cmo

