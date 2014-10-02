GRA = Kju

all: $(GRA).tokens

$(GRA).tokens: $(GRA).g4 Lexer.g4
	antlr4 $<
	javac  $(GRA)*.java

debug: $(GRA).tokens
	java org.antlr.v4.runtime.misc.TestRig $(GRA) root -gui -encoding utf8

distclean:
	$(if $(wildcard $(GRA)*.tokens), \
	  rm -f $(GRA)*.class $(GRA)*.java $(GRA)*.tokens)

test: $(GRA).tokens
	./test/check.sh examples/snippets.kju
.PHONY: test
