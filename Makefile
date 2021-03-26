
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
PACKAGES=bos,fmt,camlp5.extprint,camlp5.extend,camlp5.pprintf,pcre,yaml,pa_ppx.deriving_plugins.std,pa_ppx.testutils,sedlex

OBJ=jsontypes.cmo jsontoken.cmo jsonparse.cmo tml.cmo

all: $(OBJ) yamltest jsontest ocamlyaml_tmltest bs4j_tmltest

yamltest: $(OBJ) yamltest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

ocamlyaml_tmltest: $(OBJ) ocamlyaml_tmltest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

bs4j_tmltest: $(OBJ) bs4j_tmltest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

jsontest: $(OBJ) jsontest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

test:: all
	mkdir -p _build
	./jsontest -runner sequential || true
	./ocamlyaml_tmltest || true
	./bs4j_tmltest || true
#	./yamltest || true

.SUFFIXES: .mll .ml .cmo .cmx

jsontypes.cmo: jsontypes.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

jsontest.cmo: jsontest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<
tml.cmo: tml.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),sedlex,oUnit -syntax camlp5o -c $<

jsonparse.cmo: jsonparse.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),sedlex,oUnit -syntax camlp5r -c $<

jsontoken.cmo: jsontoken.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package camlp5.gramlib,sedlex.ppx -c $<

yamltest.cmo: yamltest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

ocamlyaml_tmltest.cmo: ocamlyaml_tmltest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

bs4j_tmltest.cmo: bs4j_tmltest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

jsontest.cmo: jsontest.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -syntax camlp5o -c $<

.mll.ml:
	ocamllex $<
#	perl -p -i -e 's,#.*,,' $@

clean:
	rm -rf *test *.cm* *.o yamllexer.ml _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o tml.ml jsontypes.ml yamltest.ml jsontest.ml ocamlyaml_tmltest.ml bs4j_tmltest.ml > .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package sedlex.ppx jsontoken.ml >> .depend.NEW || true
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r jsonparse.ml >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
