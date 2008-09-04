GHCMAKE = ghc -fglasgow-exts --make
ALEX = alex
HAPPY = happy
LEX_SPEC = Lexer.x
PARSE_SPEC = Parser.y
GENERATED = Lexer.hs Parser.hs
SOURCES = Data.hs Utils.hs Eval.hs $(GENERATED) Primitives.hs Repl.hs
EXECS = eve test lex parse

.PHONY: clean-bak clean-gen clean check

eve: Main.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

test: RunTest.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

lex: PrintTokens.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

parse: PrintParse.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

check: test
	./test

all: $(EXECS)

Lexer.hs: Data.hs $(LEX_SPEC)
	$(ALEX) $(LEX_SPEC)

Parser.hs: Data.hs $(PARSE_SPEC)
	$(HAPPY) $(PARSE_SPEC)

clean-bak:
	rm -f *~
clean-gen:
	rm -f $(GENERATED)
clean: clean-gen clean-bak
	rm -f *~ *.hi *.o $(EXECS)
