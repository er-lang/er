all: compile

compile:
	antlr4 Kju.g4
	javac Kju*.java

debug:
	java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -gui -encoding utf8

clean:
	rm -f *.class

distclean: clean
	rm -f *.java *.tokens

test:
	./test/check.sh ../examples/snippets.kju
.PHONY: test
