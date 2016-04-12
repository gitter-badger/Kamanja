package com.ligadata.HeartBeat


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


/**
 *
 * @param typ - Component type. Ex - INPUT_ADAPTER...
 * @param name - Component name
 * @param description - Description
 * @param startTime - Time the components was last successfuly started
 * @param lastSeen - Heartbeat
 * @param metricsJsonString - Generic JSON sting of all the data.
 */
case class MonitorComponentInfo(typ: String, name: String, description: String, startTime: String, lastSeen: String, metricsJsonString: String)

/**
 *  Monitorable - Base Monitor component to get Component Status & Metrics information.
 */
trait Monitorable {
  def getComponentStatusAndMetrics: MonitorComponentInfo

  /**
    * this is a very simple string to be externalized on a Status timer for the adapter.
    * @return String
    */
  def getComponentSimpleStats: String
}