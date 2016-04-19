package com.ligadata.kamanja.CheckerComponent;

import java.util.Properties;

import kafka.producer.KeyedMessage;
import kafka.javaapi.producer.Producer;
import kafka.producer.ProducerConfig;

public class KafkaProducer {
	Producer<Integer, String> producer;
	String topic = "testout_1";
	String msg = "LigaData Company";
	//kafka.initializer()
	//kafka.ceatemessage()
	public void initialize(String host){
		Properties props = new Properties();
		props.put("metadata.broker.list", host);
		props.put("serializer.class", "kafka.serializer.StringEncoder");
		props.put("request.required.acks", "1");
		ProducerConfig producerConfig = new ProducerConfig(props);
		System.out.println(producerConfig.brokerList());
		producer = new Producer<Integer, String>(producerConfig);
	}
	
	public void CreateMessage(){
		KeyedMessage<Integer, String> keyedMsg = new KeyedMessage<Integer, String>(topic, msg);
		producer.send(keyedMsg);
	}

	public Producer<Integer, String> getProducer() {
		return producer;
	}

	public void setProducer(Producer<Integer, String> producer) {
		this.producer = producer;
	}

	public String getTopic() {
		return topic;
	}

	public void setTopic(String topic) {
		this.topic = topic;
	}

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}
}
