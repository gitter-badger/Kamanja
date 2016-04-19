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

package com.ligadata.kamanja.consumer.util;

public interface ConfigFields {

	String ZK_CONNECTION = "zk_connection";
	String ALERTS_QUEUE_TOPIC = "alerts_queue_topic";
	String STATUS_QUEUE_TOPIC = "status_queue_topic";
	String ALERTS_QUEUE_THREADS_NUMBER = "alerts_queue_threads_number";
	String STATUS_QUEUE_THREADS_NUMBER = "status_queue_threads_number";
	String QUEUE_CONSUMER_GROUP_ID = "queue_consumer_group_id";
	String ALERTS_AGG_TASK_PERIOD_MILLIES = "alerts_agg_task_period_millies";
}
