bnfc = ~/.cabal/bin/bnfc 

all:
	happy -gca SPL/Parsers/ParCK.y
	alex -g SPL/Parsers/LexCK.x
	(cd SPL/Parsers/; latex DocCK.tex; dvips DocCK.dvi -o DocCK.ps)
	ghc --make SPL/Parsers/TestCK.hs -o SPL/Parsers/TestCK
clean:
	-rm -f SPL/Parsers/*.log SPL/Parsers/*.aux SPL/Parsers/*.hi SPL/Parsers/*.o SPL/Parsers/*.dvi
	-rm -f SPL/Parsers/DocCK.ps
distclean: clean
	-rm -f SPL/Parsers/DocCK.* SPL/Parsers/LexCK.* SPL/Parsers/ParCK.* SPL/Parsers/LayoutCK.* SPL/Parsers/SkelCK.* SPL/Parsers/PrintCK.* SPL/Parsers/TestCK.* SPL/Parsers/AbsCK.* SPL/Parsers/TestCK SPL/Parsers/ErrM.* SPL/Parsers/SharedString.* SPL/Parsers/CK.dtd SPL/Parsers/XMLCK.* Makefile*
	-rmdir -p SPL/Parsers/

bnfc:	$(bnfc)
	$(bnfc) -haskell -p SPL.Parsers CK.cf
