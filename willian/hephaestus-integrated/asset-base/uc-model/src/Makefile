all:
	happy -gca UseCaseModel/Parsers/BNFC/ParUCM.y
	alex -g UseCaseModel/Parsers/BNFC/LexUCM.x
	(cd UseCaseModel/Parsers/BNFC/; latex DocUCM.tex; dvips DocUCM.dvi -o DocUCM.ps)
	ghc --make UseCaseModel/Parsers/BNFC/TestUCM.hs -o UseCaseModel/Parsers/BNFC/TestUCM
clean:
	-rm -f UseCaseModel/Parsers/BNFC/*.log UseCaseModel/Parsers/BNFC/*.aux UseCaseModel/Parsers/BNFC/*.hi UseCaseModel/Parsers/BNFC/*.o UseCaseModel/Parsers/BNFC/*.dvi
	-rm -f UseCaseModel/Parsers/BNFC/DocUCM.ps
distclean: clean
	-rm -f UseCaseModel/Parsers/BNFC/DocUCM.* UseCaseModel/Parsers/BNFC/LexUCM.* UseCaseModel/Parsers/BNFC/ParUCM.* UseCaseModel/Parsers/BNFC/LayoutUCM.* UseCaseModel/Parsers/BNFC/SkelUCM.* UseCaseModel/Parsers/BNFC/PrintUCM.* UseCaseModel/Parsers/BNFC/TestUCM.* UseCaseModel/Parsers/BNFC/AbsUCM.* UseCaseModel/Parsers/BNFC/TestUCM UseCaseModel/Parsers/BNFC/ErrM.* UseCaseModel/Parsers/BNFC/SharedString.* UseCaseModel/Parsers/BNFC/UCM.dtd UseCaseModel/Parsers/BNFC/XMLUCM.* Makefile*
	-rmdir -p UseCaseModel/Parsers/BNFC/
