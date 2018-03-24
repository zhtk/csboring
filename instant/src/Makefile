all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	latex DocInstant.tex; dvips DocInstant.dvi -o DocInstant.ps
	ghc --make TestInstant.hs -o TestInstant
	ghc --make insc_jvm.hs -o insc_jvm
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocInstant.ps
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* Makefile*

