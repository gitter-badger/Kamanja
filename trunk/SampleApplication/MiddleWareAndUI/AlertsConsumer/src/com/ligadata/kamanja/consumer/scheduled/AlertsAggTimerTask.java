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

package com.ligadata.kamanja.consumer.scheduled;

import java.util.TimerTask;

import org.apache.curator.framework.CuratorFramework;

import com.ligadata.kamanja.consumer.listener.ControlKeys;
import com.ligadata.kamanja.consumer.message.Aggregations;
import com.ligadata.kamanja.consumer.util.ZKFields;
import com.ligadata.kamanja.consumer.util.ZKUtil;

public class AlertsAggTimerTask extends TimerTask {

	@Override
	public void run() {

		if (ControlKeys.start) {
			updateAllValues();
		}

	}

	private synchronized void updateAllValues() {

		CuratorFramework client = ZKUtil.getSimpleClient(null);
		Aggregations agg = Aggregations.getInstance();
		ZKUtil.setValue(client, ZKFields.EB1_ALERTS, agg.getEB1Alerts() + "");
		ZKUtil.setValue(client, ZKFields.EB2_ALERTS, agg.getEB2Alerts() + "");
		ZKUtil.setValue(client, ZKFields.EVENTS_PROCESSED,
				agg.getEventsProcessed() + "");
		ZKUtil.setValue(client, ZKFields.LATEST_MSGS, agg.getLatestMessages());
		ZKUtil.setValue(client, ZKFields.LB_ALERTS, agg.getLBAlerts() + "");
		ZKUtil.setValue(client, ZKFields.NOD_ALERTS, agg.getNODAlerts() + "");
		ZKUtil.setValue(client, ZKFields.OD1_ALERTS, agg.getOD1Alerts() + "");
		ZKUtil.setValue(client, ZKFields.OD2_ALERTS, agg.getOD2Alerts() + "");
		ZKUtil.setValue(client, ZKFields.OD3_ALERTS, agg.getOD3Alerts() + "");
		ZKUtil.setValue(client, ZKFields.TOTAL_ALERTS, agg.getTotalAlerts()
				+ "");
		ZKUtil.setValue(client, ZKFields.UTF_ALERTS, agg.getUTFAlerts() + "");

	}
}
