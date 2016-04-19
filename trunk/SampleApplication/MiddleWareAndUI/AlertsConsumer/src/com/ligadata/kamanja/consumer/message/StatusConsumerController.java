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

import kafka.consumer.KafkaStream;

public class StatusConsumerController extends AbstractConsumerController {

	public StatusConsumerController(String zookeeper, String groupId,
			String topic, int threadsNum) {
		super(zookeeper, groupId, topic, threadsNum);

	}

	@Override
	public AbstractConsumer getNewConsumer(KafkaStream<byte[], byte[]> stream,
			int threadNumbe) {

		return new StatusConsumer(stream, threadNumbe);
	}

}
