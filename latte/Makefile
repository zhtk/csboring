all:
	(cd src; happy -gca ParLatte.y)
	(cd src; alex -g LexLatte.x)
	(cd src; ghc -dynamic --make Main.hs -o ../latc_llvm)

clean:
	-(cd src; rm -f *.log *.aux *.hi *.o *.dvi)
	-rm -f latc

distclean: clean
	-(cd src; rm -f DocLatte.* LexLatte.* ParLatte.* LayoutLatte.* SkelLatte.* PrintLatte.* TestLatte.* AbsLatte.* TestLatte ErrM.* SharedString.* ComposOp.* Latte.dtd XMLLatte.* Makefile*)
	
