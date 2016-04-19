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

import kafka.consumer.ConsumerIterator;
import kafka.consumer.KafkaStream;

public abstract class AbstractConsumer implements Runnable {

	private KafkaStream<byte[], byte[]> stream;
	private int threadNumbe;

	public AbstractConsumer(KafkaStream<byte[], byte[]> stream, int threadNumbe) {

		this.stream = stream;
		this.threadNumbe = threadNumbe;
	}

	public void run() {

		ConsumerIterator<byte[], byte[]> it = stream.iterator();
		while (it.hasNext()) {

			handleMessage(new String(it.next().message()), threadNumbe);
		}
	}

	public abstract void handleMessage(String message, int threadNumber);



}
