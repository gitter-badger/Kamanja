KAMANJA_HOME={InstallDirectory}

java -cp $KAMANJA_HOME/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:$KAMANJA_HOME/lib/system/simplekafkaproducer_2.11-1.4.0.jar SimpleKafkaProducer --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "$KAMANJA_HOME/input/SampleApplications/data/copd_demo_Medical.csv.gz" --partitionkeyidxs "1" --format CSV
