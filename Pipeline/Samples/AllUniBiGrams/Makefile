MyUnitPath=../../../../my-units
ModulesPath=../../../../Modules/ParamManager/
DebugIndex?=-1
DebugStart?=-1
DebugEnd?=-1
TaskID?=-1
Debug?=0

AllUniBiGrams-Release:
	mkdir -p /tmp/AllUniBiGrams-Release
	fpc -O2 -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser -Fu$(ModulesPath) -Fu. -Fu../../ -FU/tmp/AllUniBiGrams-Release -Fu/usr/local/share/lazarus/components/*/ -oAllUniBiGrams-Release 

AllUniBiGrams-Debug:
	mkdir -p /tmp/AllUniBiGrams-Debug
	fpc -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser  -Fu$(ModulesPath) -Fu./ -Fu../ -Fu../../ -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-Debug   -oAllUniBiGrams-Debug


AllUniBiGrams-MemLeak:
	mkdir -p /tmp/AllUniBiGrams-MemLeak
	fpc -g -gh -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser  -Fu$(ModulesPath) -Fu. -Fu../../  -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-MemLeak -oAllUniBiGrams-MemLeak

AllUniBiGrams-Profiling:
	mkdir -p /tmp/AllUniBiGrams-Profiling
	fpc -O3 -gv -g -gl AllUniBiGrams.lpr -Fu$(MyUnitPath)/Pipeline -Fu$(MyUnitPath)/General-Purpose-Units -Fu$(MyUnitPath)/Unicode-Functions -Fu$(MyUnitPath)/ALogger -Fu$(MyUnitPath)/Threading -Fu$(MyUnitPath)/PB2PAS -Fu$(MyUnitPath)/Algorithmic-Units -Fu$(MyUnitPath)/WikiParser  -Fu$(ModulesPath) -Fu. -Fu../../  -Fu/usr/local/share/lazarus/components/*/ -FU/tmp/AllUniBiGrams-Profiling  -oAllUniBiGrams-Profiling
	valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes  ./AllUniBiGrams-Profiling Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=${Debug} DebugIndex=$(DebugIndex) DebugStart=$(DebugStart) DebugEnd=$(DebugEnd)  Pipeline.TaskID=${TaskID} 

MemLeak: AllUniBiGrams-MemLeak
	./AllUniBiGrams-MemLeak Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=${Debug} DebugIndex=$(DebugIndex) DebugStart=$(DebugStart) DebugEnd=$(DebugEnd)  Pipeline.TaskID=${TaskID} 

MemLeak: AllUniBiGrams-MemLeak
	./AllUniBiGrams-MemLeak Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=${Debug} DebugIndex=$(DebugIndex) DebugStart=$(DebugStart) DebugEnd=$(DebugEnd)  Pipeline.TaskID=${TaskID} 


Step1:
	${Binary} Pipeline.StepID=1 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=$(Debug) 2>&1 

Step2:
	${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=${Debug} DebugIndex=$(DebugIndex) DebugStart=$(DebugStart) DebugEnd=$(DebugEnd)  Pipeline.TaskID=$(TaskID)

TestFindStartIndices:
	${Binary} Mode=TestFindStartIndices InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/   Pipeline.TaskID=$(TaskID) Debug=$(Debug)


TestExtractContent:
	${Binary} Mode=TestExtractContent InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=${Debug} DebugIndex=$(DebugIndex) DebugStart=$(DebugStart) DebugEnd=$(DebugEnd)  Pipeline.TaskID=$(TaskID)

DStep2:
	${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=$(Debug) DebugIndex=$(DebugIndex)  2>&1

Proto:
	for f in `ls Protos/*.proto`;\
	do\
	  ~/Projects/PB2PAS/PB2PAS  --InputFile `pwd`/$$f; \
	done

clean:
	rm -Rf /tmp/AllUniBiGrams-* ./AllUniBiGrams-*
