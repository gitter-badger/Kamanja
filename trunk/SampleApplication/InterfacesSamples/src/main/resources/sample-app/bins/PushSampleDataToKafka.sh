# java -jar /tmp/KamanjaInstall/bin/SimpleKafkaProducer-0.1.0 --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "/tmp/KamanjaInstall/input/application-3/data/TxnDataOneCustomer.dat" --partitionkeyidxs "1" --format CSV

java -jar /tmp/KamanjaInstall/bin/SimpleKafkaProducer-0.1.0 --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "/tmp/KamanjaInstall/input/application-3/data/TxnData.dat" --partitionkeyidxs "1" --format CSV
