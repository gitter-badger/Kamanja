#/bin/bash

# Accept the files on the command line and send them to the kafka producer.  All files assumed to be compressed CSV

if [[ $# -gt 0 ]]; then
	DATA_FILES="$*"
	FORMAT="CSV"
	java -jar $KAMANJA_HOME/bin/SimpleKafkaProducer-0.1.0 --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "$DATA_FILES" --partitionkeyidxs "1" --format $FORMAT
else
	echo
	echo "Supply some (compressed csv) inputs if you want to test some. All files assumed to be compressed CSV."
	echo "Usage:"
	echo "   JpmmlPushKafka.sh <filepath1> [<filepath2 <filepath3> ... <filepathN>]"
	echo
fi
