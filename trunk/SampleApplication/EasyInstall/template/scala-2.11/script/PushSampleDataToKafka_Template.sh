#!/usr/bin/env bash
KAMANJA_HOME={InstallDirectory}
if [ "$#" -eq 1 ]; then
INPUTFILE=$@
else
count=0
FILEDIR=$KAMANJA_HOME/input/SampleApplications/data
for entry in "$FILEDIR"/*
do
count=$((count+1))
  echo "$count: $entry"
  LISTOFFILES[count-1]=$entry
done
read -p "Please select from the above options: " useroption
OPTION=useroption-1
INPUTFILE=${LISTOFFILES[OPTION]}
fi
echo "User selected: $INPUTFILE"
java -cp $KAMANJA_HOME/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/simplekafkaproducer_2.11-1.4.0.jar SimpleKafkaProducer --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files $INPUTFILE   --partitionkeyidxs "1" --format CSV
