TARGET       = kocaml
OCAMLOPT     = ocamlopt
OCAMLDEP     = ocamldep
OCAMLOPTFLAGS=
SRCS         = span.ml error.ml token.ml lexer.ml ast.ml parser.ml id.ml alpha.ml codegen.ml main.ml
CMX          = $(SRCS:.ml=.cmx)
DEPS         = .depend

$(TARGET): $(CMX)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $^

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

$(DEPS): $(SRCS)
	$(OCAMLDEP) -native $(SRCS) > $@

-include $(DEPS)

.PHONY: test
test: $(TARGET)
	./test.sh

.PHONY: clean
clean:
	rm -f $(TARGET) *.cm[iox] *.o $(DEPS) tmp*
