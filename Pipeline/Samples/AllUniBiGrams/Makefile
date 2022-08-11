MyUnitPath=../../../../my-units

AllUniBiGrams-Release:
	mkdir -p /tmp/AllUniBiGrams-Release
	fpc -Sd -O3 -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB1PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-Release -oAllUniBiGrams-Release

AllUniBiGrams-Debug:
	mkdir -p /tmp/AllUniBiGrams-Debug
	fpc -Sd  -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-Debug -oAllUniBiGrams-Debug


AllUniBiGrams-MemLeak:
	mkdir -p /tmp/AllUniBiGrams-MemLeak
	fpc -Sd  -g -gh -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-MemLeak -oAllUniBiGrams-MemLeak

AllUniBiGrams-Profiling:
	mkdir -p /tmp/AllUniBiGrams-Profiling
	fpc -Sd -O3 -gv -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/PB2PAS -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-Profiling -oAllUniBiGrams-Profiling
	valgrind --tool=callgrind ./AllUniBiGrams-Profiling 


clean:
	rm -Rf /tmp/AllUniBiGrams-*
