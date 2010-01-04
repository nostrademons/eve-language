GHCMAKE = ghc -fglasgow-exts --make
ALEX = alex
HAPPY = happy
LEX_SPEC = Lexer.x
PARSE_SPEC = Parser.y
GENERATED = Lexer.hs Parser.hs
SOURCES = Utils.hs SourcePos.hs Token.hs Error.hs Literal.hs Expr.hs Types.hs TypeCheck.hs CodeGen.hs $(GENERATED)
EXECS = eve

.PHONY: clean-gen clean check

eve: Main.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

repl: Repl.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

evetest: RunTest.hs $(SOURCES)
	$(GHCMAKE) $< -o $@

Lexer.hs: $(LEX_SPEC)
	$(ALEX) $(LEX_SPEC)

Parser.hs: $(PARSE_SPEC)
	$(HAPPY) $(PARSE_SPEC)

clean-gen:
	rm -f $(GENERATED)
clean: clean-gen
	rm -f *~ *.hi *.o $(EXECS)
