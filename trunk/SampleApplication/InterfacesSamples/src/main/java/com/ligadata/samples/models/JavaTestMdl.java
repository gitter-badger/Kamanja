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

package com.ligadata.samples.models;

import com.ligadata.KamanjaBase.*;
import com.ligadata.samples.messages.*;
import com.google.common.base.Optional;


//public class JavaTestMdl extends ModelBase {
/**
	static JavaTestMdlObj objSignleton = new JavaTestMdlObj();

	public ModelResultBase execute(boolean emitAllResults) {
		// Directly calling methods from Scala Singleton object. Not preferable
		// to use direct scala.
		CustAlertHistory custAlertHistory = CustAlertHistoryFactory.rddObject.getRecentOrNew();

		// Getting Java RDD Object and performing operations on that
		JavaRDDObject<CustAlertHistory> javaRddObj = CustAlertHistoryFactory.rddObject;
		Optional<CustAlertHistory> obj = javaRddObj.getRecent();

		if (obj.isPresent()) {

		} else {

		}

		return null;
	}

	public JavaTestMdl(ModelContext mdlContext) {
		super(mdlContext, objSignleton);
	}
 */
    /// FIXME: What is the Option Syntax to use for java???????????????????????????????????????????????
    /**
	public static class JavaTestMdlObj implements ModelBaseObj {
		public boolean IsValidMessage(MessageContainerBase msg, scala.Option<? extends JPMMLInfo> jPMMLInfo) {
			return (msg instanceof CustAlertHistory);
		}

		public ModelBase CreateNewModel(ModelContext mdlContext) {
			return new JavaTestMdl(mdlContext);
		}

		public String ModelName() {
			return "JavaTestMdl";
		}

		public String Version() {
			return "0.0.1";
		}

		public ModelResultBase CreateResultObject() {
			return new MappedModelResults();
		}
	}
     */
//}