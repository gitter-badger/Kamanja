java -jar /tmp/drdigital/bin/SimpleKafkaProducer-0.1.0 --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "/tmp/drdigital/input/pmmludfTests/data/gonzales-wrapper.csv.gz" --partitionkeyidxs "1" --format CSV
java -jar /tmp/drdigital/bin/SimpleKafkaProducer-0.1.0 --gz true --topics "testin_1" --threads 1 --topicpartitions 8 --brokerlist "localhost:9092" --files "/tmp/drdigital/input/pmmludfTests/data/gonzales.csv.gz" --partitionkeyidxs "1" --format CSV
