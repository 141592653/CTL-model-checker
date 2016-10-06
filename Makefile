SOURCES=*.ml
OBJECTS=$(subst .ml,.cmo,$(SOURCES))
MLIS=*.mli
CMIS=$(subst .mli,.cmi,$(MLIS))

default: $(CMIS) $(OBJECTS)
	ocamlc $(OBJECTS) -o ctlchecker

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<
