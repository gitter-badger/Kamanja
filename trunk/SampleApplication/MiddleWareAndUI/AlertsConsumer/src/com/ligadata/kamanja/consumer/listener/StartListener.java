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

package com.ligadata.kamanja.consumer.listener;

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.shared.SharedValueListener;
import org.apache.curator.framework.recipes.shared.SharedValueReader;
import org.apache.curator.framework.state.ConnectionState;

import com.ligadata.kamanja.consumer.Main;
import com.ligadata.kamanja.consumer.message.Aggregations;

public class StartListener implements SharedValueListener {

	@Override
	public void stateChanged(CuratorFramework arg0, ConnectionState arg1) {
	}

	@Override
	public void valueHasChanged(SharedValueReader reader, byte[] arg1)
			throws Exception {

		if (Integer.parseInt(new String(reader.getValue())) == 1) {
			Main.resetValuesOnZK();
			Aggregations.getInstance().resetCounters();
			ControlKeys.start = true;

		} else {
			ControlKeys.start = false;

		}

	}

}
