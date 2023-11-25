top=32768
bot=64536
Binary=./AllUniBiGrams-Debug
 ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${bot} > /tmp/$top-$bot.log 2>&1
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
  echo /tmp/$top-$mid.log 
  ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${mid} > /tmp/$top-$mid.log 2>&1

  if [ $? -ne 0 ];
  then
    echo 'failed' ${top} ${mid}
    bot=$mid
  else
    top=`expr $mid + 1`
    ${Binary} Pipeline.StepID=2 InputFile=../../../../1BWLM/NLP/Wikidump/fawiki-20230220-pages-articles-multistream.xml WorkingDir=../../../../1BWLM/NLP/tmp/ Debug=0 DebugStart=${top} DebugEnd=${bot} > /tmp/$top-$bot.log 2>&1
    echo /tmp/$top-$bot.log 
    echo $top "to" $bot "->" $?
  fi
  echo $top '->' $bot
	  
done
