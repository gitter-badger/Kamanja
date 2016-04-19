package com.ligadata.kamanja.CheckerComponent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import kafka.consumer.Consumer;
import kafka.consumer.ConsumerConfig;
import kafka.consumer.ConsumerIterator;
import kafka.consumer.KafkaStream;
import kafka.javaapi.consumer.ConsumerConnector;

public class KafkaConsumer {
	ConsumerConnector consumerConnector;
	String topic = "yousef";

	// kafkaconsumer.initialize();
	// kafkaconsumer.consume();
	public void initialize(String host) {
		Properties props = new Properties();
		props.put("zookeeper.connect",host);
		props.put("group.id", "test1");
		props.put("zookeeper.session.timeout.ms", "4000");
		props.put("zookeeper.sync.time.ms", "200");
		props.put("auto.commit.interval.ms", "1000");
		ConsumerConfig conConfig = new ConsumerConfig(props);
		consumerConnector = Consumer.createJavaConsumerConnector(conConfig);
	}

	public void consume() {
		Map<String, Integer> topicCount = new HashMap<String, Integer>();
		topicCount.put(topic, 1);

		Map<String, List<KafkaStream<byte[], byte[]>>> consumerStreams = consumerConnector
				.createMessageStreams(topicCount);

		List<KafkaStream<byte[], byte[]>> kStreamList = consumerStreams.get(topic);

		for (final KafkaStream<byte[], byte[]> kStreams : kStreamList) {
			ConsumerIterator<byte[], byte[]> consumerIte = kStreams.iterator();
			while (true) {
				if (consumerIte.hasNext()){
					System.out.println("Message consumed from topic [" + topic + "] : "
							+ new String(consumerIte.next().message()));
				}
			}
		}
		if (consumerConnector != null)
			consumerConnector.shutdown();
	}

	public ConsumerConnector getConsumerConnector() {
		return consumerConnector;
	}

	public void setConsumerConnector(ConsumerConnector consumerConnector) {
		this.consumerConnector = consumerConnector;
	}

	public String getTopic() {
		return topic;
	}

	public void setTopic(String topic) {
		this.topic = topic;
	}
}
