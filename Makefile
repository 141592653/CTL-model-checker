SOURCES=$(wildcard *.ml)
OBJECTS=$(subst .ml,.cmo,$(SOURCES))
MLIS=$(wildcard *.mli)
CMIS=$(subst .mli,.cmi,$(MLIS))

EXEC=ctlchecker

default: $(CMIS) $(OBJECTS)
	ocamlc $(OBJECTS) -o $(EXEC)

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm *.cm*
	rm $(EXEC)
