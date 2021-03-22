
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
PACKAGES=fmt,camlp5.extprint,camlp5.extend,camlp5.pprintf,pcre,yaml,pa_ppx.deriving_plugins.std,pa_ppx.testutils,sedlex

OBJ=jsontoken.cmo jsonparse.cmo

all: $(OBJ) yamltest lextest

yamltest: yamltest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

lextest: jsontoken.cmo jsonparse.cmo lextest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

test:: all
	mkdir -p _build
	./lextest -runner sequential
#	./yamltest

.SUFFIXES: .mll .ml .cmo .cmx

jsonparse.cmo: jsonparse.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),sedlex,oUnit -syntax camlp5r -c $<

jsontoken.cmo: jsontoken.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package camlp5.gramlib,sedlex.ppx -c $<

yamltest.cmo: yamltest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

lextest.cmo: lextest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

.mll.ml:
	ocamllex $<
#	perl -p -i -e 's,#.*,,' $@

clean:
	rm -rf yamltest lextest *.cm* *.o yamllexer.ml _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o yamltest.ml lextest.ml > .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package sedlex.ppx jsontoken.ml >> .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r jsonparse.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
