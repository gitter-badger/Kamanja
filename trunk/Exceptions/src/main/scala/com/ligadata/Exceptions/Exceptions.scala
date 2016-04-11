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

package com.ligadata.Exceptions

class KamanjaException(msg: String, cause: Throwable) extends Exception(msg, cause)

case class UnsupportedObjectException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class UnsupportedOperationException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class Json4sParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class FunctionListParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class FunctionParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class TypeDefListParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class TypeParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class TypeDefProcessingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ConceptListParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ConceptParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class MessageDefParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ContainerDefParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ModelDefParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class JarDefParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class ApiResultParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class UnexpectedMetadataAPIException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ObjectNotFoundException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class CreateStoreFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class UpdateStoreFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class LoadAPIConfigException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class MissingPropertyException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class InvalidPropertyException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class KryoSerializationException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class InternalErrorException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class TranIdNotFoundException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class AlreadyExistsException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ObjectNolongerExistsException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class InvalidArgumentException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class Json4sSerializationException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ZkTransactionParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class EngineConfigParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ApiArgListParsingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class JsonException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class MessageException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class MsgCompilationFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ModelCompilationFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class MissingArgumentException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class CreateKeySpaceFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class KeyNotFoundException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ConnectionFailedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class SerializerManagerException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class ProtoBufSerializationException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class KVMessageFormatingException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class CleanUtilException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class FatalAdapterException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class StorageConnectionException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class StorageFetchException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class StorageDMLException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
case class StorageDDLException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class MessagePopulationException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class NotImplementedFunctionException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)

case class DeprecatedException(msg: String, cause: Throwable) extends KamanjaException(msg, cause)
