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

import org.apache.commons.collections.buffer.CircularFifoBuffer;


public class Aggregations {

	private int UTFAlerts;
	private int EB1Alerts;
	private int EB2Alerts;
	private int NODAlerts;
	private int OD1Alerts;
	private int OD2Alerts;
	private int OD3Alerts;
	private int LBAlerts;
	private int totalAlerts;
	private int eventsProcessed;
	private CircularFifoBuffer msgsQueue;

	private static final String MESSAGES_SEPARATOR = ",";

	private static Aggregations instance;

	static {
		instance = new Aggregations();
	}

	public static Aggregations getInstance() {

		return instance;
	}

	private Aggregations() {

		msgsQueue = new CircularFifoBuffer();
	}

	public void addMessage(String msg) {
		msgsQueue.add(msg);
	}

	public synchronized String getLatestMessages() {

		StringBuilder sb = new StringBuilder("");
		Object[] msgsOjects = msgsQueue.toArray();
		for (Object o : msgsOjects) {
			sb.append((String) o + MESSAGES_SEPARATOR);
		}

		//sb.replace(sb.length() - 2, sb.length() - 1, "");

		return sb.toString();
	}

	public void add(final String type) {

		switch (type) {

		case AlertTypes.EB1_ALERTS:
			EB1Alerts++;
			totalAlerts++;
			return;
		case AlertTypes.EB2_ALERTS:
			EB2Alerts++;
			totalAlerts++;
			return;
		case AlertTypes.EVENTS_PROCESSED:
			eventsProcessed++;
			return;
		case AlertTypes.LB_ALERTS:
			LBAlerts++;
			totalAlerts++;
			return;
		case AlertTypes.NOD_ALERTS:
			NODAlerts++;
			totalAlerts++;
			return;
		case AlertTypes.OD1_ALERTS:
			OD1Alerts++;
			totalAlerts++;
			return;
		case AlertTypes.OD2_ALERTS:
			OD2Alerts++;
			totalAlerts++;
			return;
		case AlertTypes.OD3_ALERTS:
			OD3Alerts++;
			totalAlerts++;
			return;
		case AlertTypes.UTF_ALERTS:
			UTFAlerts++;
			totalAlerts++;
			return;

		}
	}

	public void resetCounters() {

		UTFAlerts = 0;
		EB1Alerts = 0;
		EB2Alerts = 0;
		NODAlerts = 0;
		OD1Alerts = 0;
		OD2Alerts = 0;
		OD3Alerts = 0;
		LBAlerts = 0;
		totalAlerts = 0;
		eventsProcessed = 0;
	}

	public int getUTFAlerts() {
		return UTFAlerts;
	}

	public int getEB1Alerts() {
		return EB1Alerts;
	}

	public int getEB2Alerts() {
		return EB2Alerts;
	}

	public int getNODAlerts() {
		return NODAlerts;
	}

	public int getOD1Alerts() {
		return OD1Alerts;
	}

	public int getOD2Alerts() {
		return OD2Alerts;
	}

	public int getOD3Alerts() {
		return OD3Alerts;
	}

	public int getLBAlerts() {
		return LBAlerts;
	}

	public int getEventsProcessed() {
		return eventsProcessed;
	}

	public int getTotalAlerts() {
		return totalAlerts;
	}

}
