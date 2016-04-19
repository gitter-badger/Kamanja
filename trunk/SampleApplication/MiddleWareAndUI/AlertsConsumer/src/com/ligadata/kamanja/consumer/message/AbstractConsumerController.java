/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.kamanja.consumer.message;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import kafka.api.PartitionOffsetRequestInfo;
import kafka.common.TopicAndPartition;
import kafka.consumer.ConsumerConfig;
import kafka.consumer.KafkaStream;
import kafka.javaapi.OffsetResponse;
import kafka.javaapi.consumer.ConsumerConnector;
import kafka.javaapi.consumer.SimpleConsumer;

public abstract class AbstractConsumerController extends Thread{

	private String zookeeper;
	private String topic;
	private String groupId;
	private ExecutorService executor;
	private int threadsNum;

	private ConsumerConnector consumerConnector;

	public AbstractConsumerController(String zookeeper, String groupId,
			String topic, int threadsNum) {

		this.zookeeper = zookeeper;
		this.topic = topic;
		this.groupId = groupId;
		this.threadsNum = threadsNum;
	}

	public void run() {

		consumerConnector = kafka.consumer.Consumer
				.createJavaConsumerConnector(createConsumerConfig());

		Map<String, Integer> topicCountMap = new HashMap<String, Integer>();
		topicCountMap.put(topic, new Integer(threadsNum));

		Map<String, List<KafkaStream<byte[], byte[]>>> consumerMap = consumerConnector
				.createMessageStreams(topicCountMap);
		List<KafkaStream<byte[], byte[]>> streams = consumerMap.get(topic);

		executor = Executors.newFixedThreadPool(threadsNum);

		int threadNumber = 0;
		for (final KafkaStream<byte[], byte[]> stream : streams) {
			AbstractConsumer consumer = getNewConsumer(stream, threadNumber);
			executor.submit(consumer);
			threadNumber++;
		}

	}

	public void shutdown() {

		if (consumerConnector != null) {
			consumerConnector.shutdown();
		}

		if (executor != null) {
			executor.shutdown();
		}
	}

	private ConsumerConfig createConsumerConfig() {

		Properties props = new Properties();
		props.put("zookeeper.connect", zookeeper);
		props.put("group.id", groupId);
		props.put("zookeeper.session.timeout.ms", "400");
		props.put("zookeeper.sync.time.ms", "200");
		props.put("auto.commit.interval.ms", "1000");
		props.put("auto.offset.reset", "largest");

		return new ConsumerConfig(props);
	}

	public static long getLastOffset(SimpleConsumer consumer, String topic,
			int partition, long whichTime, String clientName) {

		TopicAndPartition topicAndPartition = new TopicAndPartition(topic,
				partition);
		Map<TopicAndPartition, PartitionOffsetRequestInfo> requestInfo = new HashMap<TopicAndPartition, PartitionOffsetRequestInfo>();
		requestInfo.put(topicAndPartition, new PartitionOffsetRequestInfo(
				whichTime, 1));
		kafka.javaapi.OffsetRequest request = new kafka.javaapi.OffsetRequest(
				requestInfo, kafka.api.OffsetRequest.CurrentVersion(),
				clientName);
		OffsetResponse response = consumer.getOffsetsBefore(request);

		if (response.hasError()) {
			System.out
					.println("Error fetching data Offset Data the Broker. Reason: "
							+ response.errorCode(topic, partition));
			return 0;
		}

		long[] offsets = response.offsets(topic, partition);
		return offsets[0];
	}

	public abstract AbstractConsumer getNewConsumer(
			KafkaStream<byte[], byte[]> stream, int threadNumbe);

}
