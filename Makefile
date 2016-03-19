GRA = Er

all: $(GRA).tokens

$(GRA).tokens: $(GRA).g4 Lexer.g4
	java org.antlr.v4.Tool $<
	javac  $(GRA)*.java

debug: $(GRA).tokens
	java org.antlr.v4.gui.TestRig $(GRA) root -encoding utf8 -gui

clean:
	$(if $(wildcard $(GRA)*.tokens), \
	  rm -f $(GRA)*.class $(GRA)*.java $(GRA)*.tokens)

distclean: clean
	$(if $(wildcard test/_*), rm -f test/_*)

test: $(GRA).tokens
	./test/check.sh examples/snippets.er
.PHONY: test
