all: tests programs

programs: src/insc_jvm.hs src/insc_llvm.hs
	(cd src; bnfc Instant.cf)
	(cd src; alex -g LexInstant.x)
	(cd src; happy -gca ParInstant.y)
	(cd src; ghc --make insc_jvm.hs -o ../insc_jvm)
	(cd src; ghc --make insc_llvm.hs -o ../insc_llvm)

tests:	ParInstant.hs
	alex -g LexInstant.x
	ghc --make TestInstant.hs -o TestInstant

debug: info.txt

ParInstant.y LexInstant.x: src/Instant.cf
	bnfc $<

ParInstant.hs: ParInstant.y
	happy -gca $<

info.txt: src/ParInstant.y
	happy -gca src/ParInstant.y

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocInstant.ps
	-(cd src/;rm -f *.bak)

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* info.txt
	-(cd src; rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* info.txt)
	-(cd src/;rm -f Absyn.C Instant.tex Parser.H Printer.H Skeleton.H Absyn.H Instant.l Instant.y Printer.C Skeleton.C Test.C)

