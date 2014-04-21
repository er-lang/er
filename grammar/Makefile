GRA = Kju

all: compile

compile:
	antlr4 $(GRA).g4
	javac $(GRA)*.java

debug:
	java -Xmx8g org.antlr.v4.runtime.misc.TestRig $(GRA) root -gui -encoding utf8

distclean:
	$(if $(wildcard $(GRA).tokens), rm -f *.class *.java *.tokens)


test:
	./test/check.sh ../examples/snippets.kju
.PHONY: test
