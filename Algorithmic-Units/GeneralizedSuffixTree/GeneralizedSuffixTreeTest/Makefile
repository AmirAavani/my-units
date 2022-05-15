all:
	fpc -Sd ./GeneralizedSuffixTreeTest.lpr -Fu../ -Fu../../../my-units/* -Fu../../../my-units/*/* -oGeneralizedSuffixTreeTest -FU/tmp

memleak:
	fpc -gh -g -gl -Sd ./GeneralizedSuffixTreeTest.lpr -Fu../ -Fu../../../my-units/*/* -Fu../../../my-units/* -FU/tmp -Fu../../../my-units/PB2PAS -oMLeakGeneralizedSuffixTreeTest

