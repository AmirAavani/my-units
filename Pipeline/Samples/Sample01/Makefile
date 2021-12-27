MyUnitPath=../../../../my-units

Sample01-Release:
	mkdir -p /tmp/Sample01-Release
	fpc -Sd -O3 -g -gl Sample01.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB1PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/Sample01-Release -oSample01-Release


Sample01-MemLeak:
	mkdir -p /tmp/Sample01-MemLeak
	fpc -Sd  -g -gh -gl Sample01.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/Sample01-MemLeak -oSample01-MemLeak

Sample01-Profiling:
	mkdir -p /tmp/Sample01-Profiling
	fpc -Sd -O3 -gv -g -gl Sample01.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/Sample01-Profiling -oSample01-Profiling
	valgrind --tool=callgrind ./Test00-Profiling 
	valgrind --tool=callgrind ./Sample01-Profiling 


clean:
	rm -Rf /tmp/Sample01-*
