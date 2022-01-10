MyUnitPath=../../../my-units

Test01-Release:
	mkdir -p /tmp/Test01-Release
	fpc -Sd -O3 -g -gl Test01.lpr -Fu../../../my-units/Pipeline -Fu../../../my-units/General-Purpose-Units -Fu../../../my-units/Unicode-Functions -Fu../../../my-units/ALogger -Fu../../../my-units/Threading -Fu../../../my-units/PB1PAS -Fu../../../my-units/Algorithmic-Units -FuStepHandlers -FU/tmp/Test01-Release -oTest01-Release


Test01-MemLeak:
	mkdir -p /tmp/Test01-MemLeak
	fpc -Sd  -g -gh -gl Test01.lpr -Fu../../../my-units/Pipeline -Fu../../../my-units/General-Purpose-Units -Fu../../../my-units/Unicode-Functions -Fu../../../my-units/ALogger -Fu../../../my-units/Threading -Fu../../../my-units/PB2PAS -Fu../../../my-units/Algorithmic-Units -FuStepHandlers -FU/tmp/Test01-MemLeak -oTest01-MemLeak

Test01-Profiling:
	mkdir -p /tmp/Test01-Profiling
	fpc -Sd -O3 -gv -g -gl Test01.lpr -Fu../../../my-units/Pipeline -Fu../../../my-units/General-Purpose-Units -Fu../../../my-units/Unicode-Functions -Fu../../../my-units/ALogger -Fu../../../my-units/Threading -Fu../../../my-units/PB2PAS -Fu../../../my-units/Algorithmic-Units -FuStepHandlers -FU/tmp/Test01-Profiling -oTest01-Profiling
	valgrind --tool=callgrind ./Test00-Profiling 
	valgrind --tool=callgrind ./Test01-Profiling 


clean:
	rm -Rf /tmp/Test00-*
