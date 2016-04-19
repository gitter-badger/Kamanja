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

package com.ligadata.pmml.udfs

import scala.reflect.runtime.universe._

/**
 * @deprecated.  Custom Udf libraries are no longer required to derive or otherwise inherit this trait.
 * It is scheduled for removal.  Any implementations depending on this file should be modified by simply
 * removing extends clause from their UDF object.
 *
 * UdfBase trait is inherited by all UDF libraries to be used in the Pmml models executed by Kamanja.
 * It provides key access to the function members of the 'object' methods defined so they may 
 * be cataloged in the Kamanja metadata. 
 * 
 * @see UdfExtract for how function information is extracted from an object. 
 */

trait UdfBase {
	val typeMirror = runtimeMirror(this.getClass.getClassLoader)
	val instanceMirror = typeMirror.reflect(this)
	val members = instanceMirror.symbol.typeSignature.members
} 
