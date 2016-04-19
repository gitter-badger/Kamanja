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

public interface ZKFields {

	String PARENT = "/com/ligadata/biw/mt/";
	String START = PARENT + "start";
	String MESSAGE_TIMESTAMP = PARENT + "msg_timestamp";
	String EVENTS_PROCESSED = PARENT + "events_processed";
	String TOTAL_ALERTS = PARENT + "total_alerts";
	String UTF_ALERTS = PARENT + "utf_alerts";
	String EB1_ALERTS = PARENT + "eb1_alerts";
	String EB2_ALERTS = PARENT + "eb2_alerts";
	String NOD_ALERTS = PARENT + "nod_alerts";
	String OD1_ALERTS = PARENT + "od1_alerts";
	String OD2_ALERTS = PARENT + "od2_alerts";
	String OD3_ALERTS = PARENT + "od3_alerts";
	String LB_ALERTS = PARENT + "lb_alerts";
	String LATEST_MSGS = PARENT + "latest_msgs";
}
