/*
Copyright 2016 ligaDATA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
  limitations under the License.
*/

package com.ligadata.InputOutputAdapterInfo

import org.scalatest._

class AdapterConfigurationTests extends FlatSpec with BeforeAndAfter {
  private var adapterConfig: AdapterConfiguration = null
  before {
    adapterConfig = new AdapterConfiguration
  }

  "Adapter Configuration" should "be instantiated with adapter specific configration set to null" in {
    assert(adapterConfig.adapterSpecificCfg == null)
  }

  it should "be instantiated with class name set to null" in {
    assert(adapterConfig.className == null)
  }

  it should "be instantiated with dependency jars set to null" in {
    assert(adapterConfig.dependencyJars == null)
  }

  it should "be instantiated with jar name set to null" in {
    assert(adapterConfig.jarName == null)
  }

  it should "be instantiated with name set to null" in {
    assert(adapterConfig.Name == null)
  }

  it should "have TYPE_INPUT set to Input_Adapter" in {
    assert(AdapterConfiguration.TYPE_INPUT == "Input_Adapter")
  }

  it should "have TYPE_OUTPUT set to Output_Adapter" in {
    assert(AdapterConfiguration.TYPE_OUTPUT == "Output_Adapter")
  }
}