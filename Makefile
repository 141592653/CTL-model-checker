
EXEC=ctlchecker

default:
	ocamlbuild 'main.native'
	mv main.native $(EXEC)

clean:
	rm *.cm*
	rm $(EXEC)
