#!/usr/bin/env bash
KAMANJA_HOME={InstallDirectory}
java -jar $KAMANJA_HOME/bin/SimpleKafkaProducer-0.1.0 --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "$KAMANJA_HOME/input/SampleApplications/data/TxnData_Finance.dat.gz" --partitionkeyidxs "1" --format CSV