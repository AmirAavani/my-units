MyUnitPath=../../../../my-units
DebugIndex?=-1
DebugStart?=-1
DebugEnd?=-1

AllUniBiGrams-Release:
	mkdir -p /tmp/AllUniBiGrams-Release
	fpc -Sd -O3 -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-Release -Fu/usr/local/share/lazarus/components/*/ -oAllUniBiGrams-Release 

AllUniBiGrams-Debug:
	mkdir -p /tmp/AllUniBiGrams-Debug
	fpc -Sd  -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser -Fu./ -Fu../ -Fu../../ -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-Debug   -oAllUniBiGrams-Debug


AllUniBiGrams-MemLeak:
	mkdir -p /tmp/AllUniBiGrams-MemLeak
	fpc -Sd  -g -gh -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser -Fu. -Fu../../  -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-MemLeak -oAllUniBiGrams-MemLeak

AllUniBiGrams-Profiling:
	mkdir -p /tmp/AllUniBiGrams-Profiling
	fpc -Sd -O3 -gv -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser -Fu. -Fu../../  -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-Profiling  -oAllUniBiGrams-Profiling
	valgrind --tool=callgrind ./AllUniBiGrams-Profiling 

Step1:
	${Binary} --Pipeline.StepID 1 --InputFile ../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml --WorkingDir  ../../../../1BWLM/NLP/tmp/ --Debug '15' 2>&1 

Step2:
	${Binary} --Pipeline.StepID 2 --InputFile ../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml --WorkingDir  ../../../../1BWLM/NLP/tmp/ --Debug 33 --DebugIndex $(DebugIndex) --DebugStart $(DebugStart) --DebugEnd $(DebugEnd) 2>&1 

DStep2:
	${Binary} --Pipeline.StepID 2 --InputFile ../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml --WorkingDir  ../../../../1BWLM/NLP/tmp/ --Debug $(Debug) --DebugIndex $(DebugIndex)  2>&1

Proto:
	for f in `ls Protos/*.proto`;\
	do\
	  ~/Projects/PB2PAS/PB2PAS  --InputFile `pwd`/$$f; \
	done

clean:
	rm -Rf /tmp/AllUniBiGrams-*
