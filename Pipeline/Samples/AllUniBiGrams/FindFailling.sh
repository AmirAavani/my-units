Binary=./AllUniBiGrams-Debug
taskID=28
top=4100
bot=4200

if [ $taskID -eq -1 ];
then
    for t in `seq 1 64`; do
      echo 't=' $t
      ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 Pipeline.TaskID=$t > /tmp/$t.log 2>&1
      if [ $? -ne 0 ];
      then
        taskID=`expr $t`
        break
      fi
    done;
fi

top=0
bot=1048576
${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${bot} Pipeline.TaskID=$taskID > /tmp/$top-$bot-$taskID.log 2>&1
if [ $? -eq 0 ];
then
  exit 0
fi

while [ $top -lt $bot ]
do
  echo $top, '->', $bot
  mid=`expr $top + $bot`
  mid=`expr $mid / 2`
  echo $top, $bot '->' $mid
  echo /tmp/$top-$mid-$taskID.log 
  echo ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${mid}  Pipeline.TaskID=$taskID 
  ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${mid}  Pipeline.TaskID=$taskID > /tmp/$top-$mid-$taskID.log 2>&1

  if [ $? -ne 0 ];
  then
    echo 'failed' ${top} ${mid} $taskID
    bot=$mid
  else
    top=`expr $mid + 1`
    ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20240301-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${bot}  Pipeline.TaskID=$taskID > /tmp/$top-$bot-$taskID.log 2>&1
    echo /tmp/$top-$bot-$taskID.log 
    echo $top "to" $bot "->" $?
  fi
  echo $top '->' $bot
	  
done
