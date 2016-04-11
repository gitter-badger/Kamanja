package com.ligadata.MetadataAPI

import java.util.Date

import com.ligadata.Exceptions._
import com.ligadata.Serialize.TypeDef
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadataload.MetadataLoad
import org.apache.logging.log4j.LogManager
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._


/**
  * Created by Ahmed-Work on 3/15/2016.
  */


object MetadataAPISerialization {

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  implicit val jsonFormats: Formats = DefaultFormats


  def serializeObjectToJson(mdObj: Any): String = {
    var outputJson = ""
    try {
      mdObj match {
        case o: ModelDef => {
          val json = "Model" ->
            ("Name" -> o.name) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("JarName" -> getEmptyIfNull(o.jarName)) ~
              ("NameSpace" -> o.nameSpace) ~
              ("ModelType" -> o.miningModelType.toString) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("ModelRep" -> o.modelRepresentation.toString) ~
              ("OrigDef" -> o.OrigDef) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("Description" -> o.Description) ~
              ("ModelConfig" -> o.modelConfig) ~
              ("Author" -> o.Author) ~
              ("inputMsgSets" -> o.inputMsgSets.toList.map(m => m.toList.map(f => ("Origin" -> f.origin) ~ ("Message" -> f.message) ~ ("Attributes" -> f.attributes.toList)))) ~
              ("OutputMsgs" -> getEmptyArrayIfNull(o.outputMsgs).toList) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("BooleanTypes" -> ("IsActive" -> o.IsActive) ~ ("IsReusable" -> o.isReusable) ~ ("IsDeleted" -> o.IsDeleted) ~ ("SupportsInstanceSerialization" -> o.SupportsInstanceSerialization)
                )
          outputJson = compact(render(json))
        }
        case o: MessageDef => {
          var primaryKeys = List[(String, List[String])]()
          var foreignKeys = List[(String, List[String], String, List[String])]()
          if (o.cType.Keys != null && o.cType.Keys.size != 0) {
            o.cType.Keys.foreach(m => {
              if (m.KeyType == RelationKeyType.tPrimary) {
                val pr = m.asInstanceOf[PrimaryKey]
                primaryKeys ::=(pr.constraintName, pr.key.toList)
              } else {
                val fr = m.asInstanceOf[ForeignKey]
                foreignKeys ::=(fr.constraintName, fr.key.toList, fr.forignContainerName, fr.forignKey.toList)
              }
            })
          }

          val containerDef = o.cType.asInstanceOf[ContainerTypeDef]

          val attribs =
            if (containerDef.IsFixed) {
              containerDef.asInstanceOf[StructTypeDef].memberDefs.toList
            } else {
              containerDef.asInstanceOf[MappedMsgTypeDef].attrMap.map(kv => kv._2).toList
            }

          val json = "Message" ->
            ("Name" -> o.Name) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("JarName" -> getEmptyIfNull(o.JarName)) ~
              ("NameSpace" -> o.NameSpace) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("OrigDef" -> o.OrigDef) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("CreationTime" -> o.CreationTime) ~
              ("Author" -> o.Author) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("SchemaId" -> o.cType.schemaId) ~
              ("AvroSchema" -> o.cType.avroSchema) ~
              ("PartitionKey" -> getEmptyArrayIfNull(o.cType.PartitionKey).toList) ~
              ("IsActive" -> o.IsActive) ~
              ("IsDeleted" -> o.IsDeleted) ~
              ("Persist" -> o.cType.Persist) ~
              ("Description" -> getEmptyIfNull(o.Description)) ~
              ("MsgAttributes" -> attribs.map(a =>
                ("NameSpace" -> a.NameSpace) ~
                  ("Name" -> a.Name) ~
                  ("TypNameSpace" -> a.typeDef.NameSpace) ~
                  ("TypName" -> a.typeDef.Name) ~
                  ("Version" -> a.Version) ~
                  ("CollectionType" -> ObjType.asString(a.CollectionType)))) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("PrimaryKeys" -> primaryKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2))) ~
              ("ForeignKeys" -> foreignKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2) ~ ("forignContainerName" -> m._3) ~ ("forignKey" -> m._4)))
          outputJson = compact(render(json))
        }
        case o: ContainerDef => {
          var primaryKeys = List[(String, List[String])]()
          var foreignKeys = List[(String, List[String], String, List[String])]()
          if (o.cType.Keys != null && o.cType.Keys.size != 0) {
            o.cType.Keys.foreach(m => {
              if (m.KeyType == RelationKeyType.tPrimary) {
                val pr = m.asInstanceOf[PrimaryKey]
                primaryKeys ::=(pr.constraintName, pr.key.toList)
              } else {
                val fr = m.asInstanceOf[ForeignKey]
                foreignKeys ::=(fr.constraintName, fr.key.toList, fr.forignContainerName, fr.forignKey.toList)
              }
            })
          }

          val containerDef = o.cType.asInstanceOf[ContainerTypeDef]

          val attribs =
            if (containerDef.IsFixed) {
              containerDef.asInstanceOf[StructTypeDef].memberDefs.toList
            } else {
              containerDef.asInstanceOf[MappedMsgTypeDef].attrMap.map(kv => kv._2).toList
            }

          val json = "Container" ->
            ("Name" -> o.name) ~
              ("PhysicalName" -> o.physicalName) ~
              ("JarName" -> getEmptyIfNull(o.jarName)) ~
              ("NameSpace" -> o.nameSpace) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("OrigDef" -> o.OrigDef) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("CreationTime" -> o.CreationTime) ~
              ("Author" -> o.Author) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("SchemaId" -> o.cType.schemaId) ~
              ("AvroSchema" -> o.cType.avroSchema) ~
              ("Persist" -> o.cType.Persist) ~
              ("PartitionKey" -> getEmptyArrayIfNull(o.cType.PartitionKey).toList) ~
              ("IsActive" -> o.IsActive) ~
              ("IsDeleted" -> o.IsDeleted) ~
              ("Description" -> o.Description) ~
              ("MsgAttributes" -> attribs.map(a =>
                ("NameSpace" -> a.NameSpace) ~
                  ("Name" -> a.Name) ~
                  ("TypNameSpace" -> a.typeDef.NameSpace) ~
                  ("TypName" -> a.typeDef.Name) ~
                  ("Version" -> a.Version) ~
                  ("CollectionType" -> ObjType.asString(a.CollectionType)))) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("PrimaryKeys" -> primaryKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2))) ~
              ("ForeignKeys" -> foreignKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2) ~ ("forignContainerName" -> m._3) ~ ("forignKey" -> m._4)))
          outputJson = compact(render(json))
        }
        case o: FunctionDef => {

          val json = "Function" ->
            ("Name" -> o.name) ~
              ("PhysicalName" -> o.physicalName) ~
              ("JarName" -> getEmptyIfNull(o.jarName)) ~
              ("NameSpace" -> o.nameSpace) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("OrigDef" -> o.OrigDef) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("Author" -> o.Author) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("Arguments" -> o.args.toList.map { arg => (("ArgName" -> arg.name) ~ ("ArgTypeNameSpace" -> arg.Type.nameSpace) ~ ("ArgTypeName" -> arg.Type.name)) }) ~
              ("Features" -> o.features.toList.map(m => m.toString)) ~
              ("ReturnTypeNameSpace" -> o.retType.nameSpace) ~
              ("ReturnTypeName" -> o.retType.name) ~
              ("ClassName" -> getEmptyIfNull(o.className)) ~
              ("Description" -> getEmptyIfNull(o.Description)) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("IsActive" -> o.IsActive) ~
              ("IsDeleted" -> o.IsDeleted)
          outputJson = compact(render(json))

        }
        case o: MapTypeDef => {

          val json = "MapType" ->
            ("Name" -> o.Name) ~
              ("NameSpace" -> o.NameSpace) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
              ("JarName" -> getEmptyIfNull(o.JarName)) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("Implementation" -> o.implementationName) ~
              ("ValueTypeNameSpace" -> o.valDef.NameSpace) ~
              ("ValueTypeName" -> o.valDef.Name) ~
              ("ValueTypeTType" -> ObjType.asString(o.valDef.tType)) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("OrigDef" -> o.OrigDef) ~
              ("Author" -> o.Author) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("Description" -> getEmptyIfNull(o.Description)) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("IsActive" -> o.IsActive) ~
              ("IsFixed" -> o.IsFixed) ~
              ("IsDeleted" -> o.IsDeleted)
          outputJson = compact(render(json))


        }
        case o: ArrayTypeDef => {

          val json = "ArrayType" ->
            ("Name" -> o.Name) ~
              ("NameSpace" -> o.NameSpace) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("TypeTypeName" -> ObjTypeType.asString(o.elemDef.tTypeType)) ~
              ("TypeName" -> o.elemDef.Name) ~
              ("TypeNameSpace" -> o.elemDef.NameSpace) ~
              ("TypeTType" -> ObjType.asString(o.elemDef.tType)) ~
              ("NumberOfDimensions" -> o.arrayDims) ~
              ("JarName" -> getEmptyIfNull(o.JarName)) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("Implementation" -> o.implementationName) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("OrigDef" -> o.OrigDef) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("Author" -> o.Author) ~
              ("Description" -> getEmptyIfNull(o.Description)) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("IsActive" -> o.IsActive) ~
              ("IsFixed" -> o.IsFixed) ~
              ("IsDeleted" -> o.IsDeleted)
          outputJson = compact(render(json))


        }
        /*  case o: ArrayBufTypeDef => {
            val json = "ArrayBufType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.elemDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.elemDef.tType)) ~
                ("TypeNameSpace" -> o.elemDef.nameSpace) ~
                ("NumberOfDimensions" -> o.arrayDims) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> getEmptyIfNull(o.Description)) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: SetTypeDef => {
            val json = "SetType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.keyDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("TypeNameSpace" -> o.keyDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: ImmutableSetTypeDef => {
            val json = "ImmutableSetType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.keyDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("TypeNameSpace" -> o.keyDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: TreeSetTypeDef => {
            val json = "TreeSetType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.keyDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("TypeNameSpace" -> o.keyDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: SortedSetTypeDef => {
            val json = "SortedSetType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.keyDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("TypeNameSpace" -> o.keyDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: ImmutableMapTypeDef => {
            val json = "ImmutableMapType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("KeyTypeNameSpace" -> o.keyDef.nameSpace) ~
                ("KeyTypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("ValueTypeNameSpace" -> o.valDef.nameSpace) ~
                ("ValueTypeName" -> ObjType.asString(o.valDef.tType)) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("Author" -> o.Author) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: HashMapTypeDef => {
            val json = "HashMapType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("KeyTypeNameSpace" -> o.keyDef.nameSpace) ~
                ("KeyTypeName" -> ObjType.asString(o.keyDef.tType)) ~
                ("ValueTypeNameSpace" -> o.valDef.nameSpace) ~
                ("ValueTypeName" -> ObjType.asString(o.valDef.tType)) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("Author" -> o.Author) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: ListTypeDef => {
            val json = "ListType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.valDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.valDef.tType)) ~
                ("TypeNameSpace" -> o.valDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: QueueTypeDef => {
            val json = "QueueType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TypeTypeName" -> ObjTypeType.asString(o.valDef.tTypeType)) ~
                ("TypeName" -> ObjType.asString(o.valDef.tType)) ~
                ("TypeNameSpace" -> o.valDef.nameSpace) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }
          case o: TupleTypeDef => {
            val json = "TupleType" ->
              ("Name" -> o.Name) ~
                ("NameSpace" -> o.NameSpace) ~
                ("PhysicalName" -> o.PhysicalName) ~
                ("TupleInfo" -> o.tupleDefs.toList.map(m => ("TypeNameSpace" -> m.nameSpace) ~ ("TypeName" -> m.name))) ~
                ("JarName" -> getEmptyIfNull(o.JarName)) ~
                ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
                ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
                ("Implementation" -> o.implementationName) ~
                ("ObjectDefinition" -> o.ObjectDefinition) ~
                ("OrigDef" -> o.OrigDef) ~
                ("OwnerId" -> o.OwnerId) ~
                ("TenantId" -> o.TenantId) ~
                ("Author" -> o.Author) ~
                ("Description" -> o.Description) ~
                ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
                ("IsActive" -> o.IsActive) ~
                ("IsFixed" -> o.IsFixed) ~
                ("IsDeleted" -> o.IsDeleted)
            outputJson = compact(render(json))


          }  */
        case o: JarDef => {
          val json = "Jar" ->
            ("IsActive" -> o.IsActive) ~
              ("IsDeleted" -> o.IsDeleted) ~
              ("OrigDef" -> o.OrigDef) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("NameSpace" -> o.NameSpace) ~
              ("Name" -> o.Name) ~
              ("Author" -> o.Author) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("JarName" -> getEmptyIfNull(o.jarName)) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("Description" -> o.description)
          outputJson = compact(render(json))

        }
        case o: ConfigDef => {
          val json = "Config" ->
            ("IsActive" -> o.IsActive) ~
              ("IsDeleted" -> o.IsDeleted) ~
              ("OrigDef" -> o.OrigDef) ~
              ("ObjectDefinition" -> o.ObjectDefinition) ~
              ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
              ("NameSpace" -> o.NameSpace) ~
              ("Contents" -> o.contents) ~
              ("OwnerId" -> o.OwnerId) ~
              ("TenantId" -> o.TenantId) ~
              ("Name" -> o.Name) ~
              ("Author" -> o.Author) ~
              ("PhysicalName" -> o.PhysicalName) ~
              ("JarName" -> getEmptyIfNull(o.jarName)) ~
              ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
              ("NumericTypes" -> ("Version" -> o.Version) ~ ("TransId" -> o.TranId) ~ ("UniqId" -> o.UniqId) ~ ("CreationTime" -> o.CreationTime) ~ ("ModTime" -> o.ModTime) ~ ("MdElemStructVer" -> o.MdElemStructVer) ~ ("MdElementId" -> o.MdElementId)) ~
              ("Description" -> o.description)
          outputJson = compact(render(json))

        }
        case o: NodeInfo => {
          val json = "Node" ->
            ("NodeId" -> getEmptyIfNull(o.NodeId)) ~
              ("NodePort" -> o.NodePort) ~
              ("NodeIpAddr" -> getEmptyIfNull(o.NodeIpAddr)) ~
              ("JarPaths" -> getEmptyArrayIfNull(o.JarPaths).toList) ~
              ("Scala_home" -> getEmptyIfNull(o.Scala_home)) ~
              ("Java_home" -> getEmptyIfNull(o.Java_home)) ~
              ("Classpath" -> getEmptyIfNull(o.Classpath)) ~
              ("ClusterId" -> getEmptyIfNull(o.ClusterId)) ~
              ("Power" -> o.Power) ~
              ("Roles" -> getEmptyArrayIfNull(o.Roles).toList) ~
              ("Description" -> getEmptyIfNull(o.Description))
          outputJson = compact(render(json))

        }
        case o: ClusterInfo => {
          val json = "Cluster" ->
            ("ClusterId" -> getEmptyIfNull(o.ClusterId)) ~
              ("Description" -> getEmptyIfNull(o.Description)) ~
              ("Privileges" -> getEmptyIfNull(o.Privileges))
          outputJson = compact(render(json))

        }
        case o: ClusterCfgInfo => {
          val json = "ClusterCfg" ->
            ("ClusterId" -> getEmptyIfNull(o.ClusterId)) ~
              ("CfgMap" -> getEmptyHashMapIfNull(o.CfgMap).toList.map(m => ("Key" -> m._1) ~ ("Value" -> m._2))) ~
              ("ModifiedTime" -> getZeroIfDateIsNull((o.ModifiedTime))) ~
              ("CreatedTime" -> getZeroIfDateIsNull(o.CreatedTime)) ~
              ("UsrConfigs" -> getEmptyHashMapIfNull(o.getUsrConfigs).toList.map(m => ("Key" -> m._1) ~ ("Value" -> m._2)))
          outputJson = compact(render(json))

        }
        case o: AdapterInfo => {
          val json = "Adapter" ->
            ("Name" -> getEmptyIfNull(o.Name)) ~
              ("TypeString" -> getEmptyIfNull(o.TypeString)) ~
              ("ClassName" -> getEmptyIfNull(o.ClassName)) ~
              ("JarName" -> getEmptyIfNull(o.JarName)) ~
              ("DependencyJars" -> getEmptyArrayIfNull(o.DependencyJars).toList) ~
              ("AdapterSpecificCfg" -> getEmptyIfNull(o.AdapterSpecificCfg)) ~
              ("TenantId" -> o.TenantId) ~
              ("FullAdapterConfig" -> getEmptyIfNull(o.FullAdapterConfig))
          outputJson = compact(render(json))

        }
        case o: TenantInfo => {
          val primaryDataStore = if (o.primaryDataStore == null) "" else o.primaryDataStore
          val cacheConfig = if (o.cacheConfig == null) "" else o.cacheConfig
          val json = "Tenant" ->
            ("TenantId" -> o.tenantId) ~
              ("Description" -> getEmptyIfNull(o.description)) ~
              ("PrimaryDataStore" -> primaryDataStore) ~
              ("CacheConfig" -> cacheConfig)
          outputJson = compact(render(json))

        }
        case o: UserPropertiesInfo => {
          val json = "UserProperties" ->
            ("ClusterId" -> o.ClusterId) ~
              ("Props" -> getEmptyHashMapIfNull(o.Props).toList.map(m => ("Key" -> m._1) ~ ("Value" -> m._2)))
          outputJson = compact(render(json))

        }
        case _ => {
          throw new Exception("serializeObjectToJson doesn't support the objects of type " + mdObj.getClass().getName() + " yet.")
        }
      }
      logger.debug("serialized object : ", outputJson)
      outputJson
    } catch {
      case e: Exception => {
        logger.debug("Failed to serialize", e)
        throw e
      }
    }
  }

  def deserializeMetadata(metadataJson: String): Any = {

    logger.debug("Parsing json : " + metadataJson)

    val json = parse(metadataJson)

    val key = json.mapField(k => {
      (k._1, JString(""))
    }).extract[(String, String)]._1

    try {
      key match {
        case "Model" => parseModelDef(json)
        case "Message" => parseMessageDef(json)
        case "Container" => parseContainerDef(json)
        case "Function" => parseFunctionDef(json)
        case "MapType" => parseMapTypeDef(json)
        case "ArrayType" => parseArrayTypeDef(json)
       /* case "ArrayBufType" => parseArrayBufTypeDef(json)
        case "SetType" => parseSetTypeDef(json)
        case "ImmutableSetType" => parseImmutableSetTypeDef(json)
        case "TreeSetType" => parseTreeSetTypeDef(json)
        case "SortedSetType" => parseSortedSetTypeDef(json)
        case "ImmutableMapType" => parseImmutableMapTypeDef(json)
        case "HashMapType" => parseHashMapTypeDef(json)
        case "ListType" => parseListTypeDef(json)
        case "QueueType" => parseQueueTypeDef(json)
        case "TupleType" => parseTupleTypeDef(json)*/
        case "Jar" => parseJarDef(json)
        case "Config" => parseConfigDef(json)
        case "Node" => parseNodeInfo(json)
        case "Cluster" => parseClusterInfo(json)
        case "ClusterCfg" => parseClusterCfgInfo(json)
        case "Adapter" => parseAdapterInfo(json)
        case "Tenant" => parseTenantInfo(json)
        case "UserProperties" => parseUserPropertiesInfo(json)
        case _ => throw new Exception("deserializeMetadata doesn't support the objects of type objectType of " + key + " yet.")
      }
    } catch {
      case e: Exception => {
        logger.debug("Failed to deserialize", e)
        throw e
      }
    }
  }

  private def parseModelDef(modDefJson: JValue): ModelDef = {
    try {

      logger.debug("Parsed the json : " + modDefJson)

      val ModDefInst = modDefJson.extract[ModelDefinition]

      val inputMsgSets = ModDefInst.Model.inputMsgSets.map(m => m.map(k => {
        val msgAndAttrib = new MessageAndAttributes()
        msgAndAttrib.message = k.Message
        msgAndAttrib.origin = k.Origin
        msgAndAttrib.attributes = k.Attributes.toArray
        msgAndAttrib
      }).toArray).toArray


      val modDef = MdMgr.GetMdMgr.MakeModelDef(ModDefInst.Model.NameSpace
        , ModDefInst.Model.Name
        , ModDefInst.Model.PhysicalName
        , ModDefInst.Model.OwnerId
        , ModDefInst.Model.TenantId
        , ModDefInst.Model.NumericTypes.UniqId
        , ModDefInst.Model.NumericTypes.MdElementId
        , ModelRepresentation.modelRep(ModDefInst.Model.ModelRep)
        , inputMsgSets
        , ModDefInst.Model.OutputMsgs.toArray
        , ModDefInst.Model.BooleanTypes.IsReusable
        , ModDefInst.Model.ObjectDefinition
        , MiningModelType.modelType(ModDefInst.Model.ModelType)
        , ModDefInst.Model.NumericTypes.Version
        , ModDefInst.Model.JarName
        , ModDefInst.Model.DependencyJars.toArray
        , false
        , ModDefInst.Model.BooleanTypes.SupportsInstanceSerialization,
        ModDefInst.Model.ModelConfig)


      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(ModDefInst.Model.ObjectFormat)
      modDef.ObjectFormat(objFmt)
      modDef.tranId = ModDefInst.Model.NumericTypes.TransId
      modDef.origDef = ModDefInst.Model.OrigDef
      modDef.creationTime = ModDefInst.Model.NumericTypes.CreationTime
      modDef.modTime = ModDefInst.Model.NumericTypes.ModTime
      modDef.description = ModDefInst.Model.Description
      modDef.author = ModDefInst.Model.Author
      modDef.mdElemStructVer = ModDefInst.Model.NumericTypes.MdElemStructVer
      modDef.active = ModDefInst.Model.BooleanTypes.IsActive
      modDef.deleted = ModDefInst.Model.BooleanTypes.IsDeleted

      modDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ModelDefParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseMessageDef(msgDefJson: JValue): MessageDef = {
    try {
      logger.debug("Parsed the json : " + msgDefJson)

      val MsgDefInst = msgDefJson.extract[MessageDefin]
      val attrList = MsgDefInst.Message.MsgAttributes
      var attrList1 = List[(String, String, String, String, Boolean, String)]()
      for (attr <- attrList) {
        attrList1 ::=(attr.NameSpace, attr.Name, attr.TypNameSpace, attr.TypName, false, attr.CollectionType)
      }

      var primaryKeys = List[(String, List[String])]()
      val pr = MsgDefInst.Message.PrimaryKeys

      pr.foreach(f => {
        primaryKeys ::=(f.constraintName, f.key)
      })

      var foreignKeys = List[(String, List[String], String, List[String])]()
      val fr = MsgDefInst.Message.ForeignKeys

      fr.foreach(f => {
        foreignKeys ::=(f.constraintName, f.key, f.forignContainerName, f.forignKey)
      })
      val msgDef = MdMgr.GetMdMgr.MakeFixedMsg(
        MsgDefInst.Message.NameSpace,
        MsgDefInst.Message.Name,
        MsgDefInst.Message.PhysicalName,
        attrList1,
        MsgDefInst.Message.OwnerId,
        MsgDefInst.Message.TenantId,
        MsgDefInst.Message.NumericTypes.UniqId,
        MsgDefInst.Message.NumericTypes.MdElementId,
        MsgDefInst.Message.SchemaId,
        MsgDefInst.Message.AvroSchema,
        MsgDefInst.Message.NumericTypes.Version,
        MsgDefInst.Message.JarName,
        MsgDefInst.Message.DependencyJars.toArray,
        primaryKeys,
        foreignKeys,
        MsgDefInst.Message.PartitionKey.toArray,
        false, MsgDefInst.Message.Persist
      )

      msgDef.tranId = MsgDefInst.Message.NumericTypes.TransId
      msgDef.origDef = MsgDefInst.Message.OrigDef
      msgDef.ObjectDefinition(MsgDefInst.Message.ObjectDefinition)
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(MsgDefInst.Message.ObjectFormat)
      msgDef.ObjectFormat(objFmt)
      msgDef.creationTime = MsgDefInst.Message.NumericTypes.CreationTime
      msgDef.modTime = MsgDefInst.Message.NumericTypes.ModTime
      msgDef.description = MsgDefInst.Message.Description
      msgDef.author = MsgDefInst.Message.Author
      msgDef.mdElemStructVer = MsgDefInst.Message.NumericTypes.MdElemStructVer
      msgDef.cType.persist = MsgDefInst.Message.Persist
      msgDef.active = MsgDefInst.Message.IsActive
      msgDef.deleted = MsgDefInst.Message.IsDeleted
      msgDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw MessageDefParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseContainerDef(contDefJson: JValue): ContainerDef = {
    try {

      logger.debug("Parsed the json : " + contDefJson)

      val ContDefInst = contDefJson.extract[ContainerDefin]
      val attrList = ContDefInst.Container.MsgAttributes
      var attrList1 = List[(String, String, String, String, Boolean, String)]()
      for (attr <- attrList) {
        attrList1 ::=(attr.NameSpace, attr.Name, attr.TypNameSpace, attr.TypName, false, attr.CollectionType)
      }

      var primaryKeys = List[(String, List[String])]()
      val pr = ContDefInst.Container.PrimaryKeys

      pr.foreach(m => {
        primaryKeys ::=(m.constraintName, m.key)
      })
      var foreignKeys = List[(String, List[String], String, List[String])]()
      val fr = ContDefInst.Container.ForeignKeys

      fr.foreach(f => {
        foreignKeys ::=(f.constraintName, f.key, f.forignContainerName, f.forignKey)
      })


      val contDef = MdMgr.GetMdMgr.MakeFixedContainer(
        ContDefInst.Container.NameSpace,
        ContDefInst.Container.Name,
        ContDefInst.Container.PhysicalName,
        attrList1,
        ContDefInst.Container.OwnerId,
        ContDefInst.Container.TenantId,
        ContDefInst.Container.NumericTypes.UniqId,
        ContDefInst.Container.NumericTypes.MdElementId,
        ContDefInst.Container.SchemaId,
        ContDefInst.Container.AvroSchema,
        ContDefInst.Container.NumericTypes.Version,
        ContDefInst.Container.JarName,
        ContDefInst.Container.DependencyJars.toArray,
        primaryKeys,
        foreignKeys,
        ContDefInst.Container.PartitionKey.toArray,
        false, ContDefInst.Container.Persist
      )

      contDef.tranId = ContDefInst.Container.NumericTypes.TransId
      contDef.origDef = ContDefInst.Container.OrigDef
      contDef.ObjectDefinition(ContDefInst.Container.ObjectDefinition)
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(ContDefInst.Container.ObjectFormat)
      contDef.ObjectFormat(objFmt)
      contDef.creationTime = ContDefInst.Container.NumericTypes.CreationTime
      contDef.modTime = ContDefInst.Container.NumericTypes.ModTime
      contDef.description = ContDefInst.Container.Description
      contDef.author = ContDefInst.Container.Author
      contDef.mdElemStructVer = ContDefInst.Container.NumericTypes.MdElemStructVer
      contDef.cType.persist = ContDefInst.Container.Persist
      contDef.active = ContDefInst.Container.IsActive
      contDef.deleted = ContDefInst.Container.IsDeleted
      contDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ContainerDefParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseFunctionDef(functionDefJson: JValue): FunctionDef = {
    try {

      logger.debug("Parsed the json : " + functionDefJson)

      val functionInst = functionDefJson.extract[Function]
      val argList = functionInst.Function.Arguments.map(arg => (arg.ArgName, arg.ArgTypeNameSpace, arg.ArgTypeName))
      var featureSet: scala.collection.mutable.Set[FcnMacroAttr.Feature] = scala.collection.mutable.Set[FcnMacroAttr.Feature]()
      if (functionInst.Function.Features != null) {
        functionInst.Function.Features.foreach(arg => featureSet += FcnMacroAttr.fromString(arg))
      }

      val functionDef = MdMgr.GetMdMgr.MakeFunc(functionInst.Function.NameSpace,
        functionInst.Function.Name,
        functionInst.Function.PhysicalName,
        (functionInst.Function.ReturnTypeNameSpace, functionInst.Function.ReturnTypeName),
        argList,
        featureSet,
        functionInst.Function.OwnerId,
        functionInst.Function.TenantId,
        functionInst.Function.NumericTypes.UniqId,
        functionInst.Function.NumericTypes.MdElementId,
        functionInst.Function.NumericTypes.Version,
        functionInst.Function.JarName,
        functionInst.Function.DependencyJars.toArray)
      functionDef.origDef = functionInst.Function.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(functionInst.Function.ObjectFormat)
      functionDef.ObjectFormat(objFmt)
      functionDef.ObjectDefinition(functionInst.Function.ObjectDefinition)
      functionDef.author = functionInst.Function.Author
      functionDef.className = functionInst.Function.ClassName
      functionDef.description = functionInst.Function.Description
      functionDef.tranId = functionInst.Function.NumericTypes.TransId
      functionDef.creationTime = functionInst.Function.NumericTypes.CreationTime
      functionDef.modTime = functionInst.Function.NumericTypes.ModTime
      functionDef.mdElemStructVer = functionInst.Function.NumericTypes.MdElemStructVer
      functionDef.active = functionInst.Function.IsActive
      functionDef.deleted = functionInst.Function.IsDeleted

      functionDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw FunctionParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseMapTypeDef(mapTypeDefJson: JValue): MapTypeDef = {
    try {

      logger.debug("Parsed the json : " + mapTypeDefJson)

      val mapTypeInst = mapTypeDefJson.extract[MapType]

      val mapTypeDef = MdMgr.GetMdMgr.MakeMap(mapTypeInst.MapType.NameSpace,
        mapTypeInst.MapType.Name,
        mapTypeInst.MapType.ValueTypeNameSpace, mapTypeInst.MapType.ValueTypeName,
        mapTypeInst.MapType.NumericTypes.Version,
        mapTypeInst.MapType.OwnerId,
        mapTypeInst.MapType.TenantId,
        mapTypeInst.MapType.NumericTypes.UniqId,
        mapTypeInst.MapType.NumericTypes.MdElementId,
        false
      )

      mapTypeDef.origDef = mapTypeInst.MapType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(mapTypeInst.MapType.ObjectFormat)
      mapTypeDef.ObjectFormat(objFmt)
      mapTypeDef.author = mapTypeInst.MapType.Author
      mapTypeDef.description = mapTypeInst.MapType.Description
      mapTypeDef.tranId = mapTypeInst.MapType.NumericTypes.TransId
      mapTypeDef.creationTime = mapTypeInst.MapType.NumericTypes.CreationTime
      mapTypeDef.modTime = mapTypeInst.MapType.NumericTypes.ModTime
      mapTypeDef.mdElemStructVer = mapTypeInst.MapType.NumericTypes.MdElemStructVer
      mapTypeDef.active = mapTypeInst.MapType.IsActive
      mapTypeDef.deleted = mapTypeInst.MapType.IsDeleted
      mapTypeDef.implementationName(mapTypeInst.MapType.Implementation)
      mapTypeDef.dependencyJarNames = mapTypeInst.MapType.DependencyJars.toArray
      mapTypeDef.jarName = mapTypeInst.MapType.JarName
      mapTypeDef.PhysicalName(mapTypeInst.MapType.PhysicalName)
      mapTypeDef.ObjectDefinition(mapTypeInst.MapType.ObjectDefinition)

      mapTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseArrayTypeDef(arrayTypeDefJson: JValue): ArrayTypeDef = {
    try {

      logger.debug("Parsed the json : " + arrayTypeDefJson)

      val arrayTypeInst = arrayTypeDefJson.extract[ArrayType]

      val arrayTypeDef = MdMgr.GetMdMgr.MakeArray(arrayTypeInst.ArrayType.NameSpace,
        arrayTypeInst.ArrayType.Name,
        arrayTypeInst.ArrayType.TypeNameSpace,
        arrayTypeInst.ArrayType.TypeName,
        arrayTypeInst.ArrayType.NumberOfDimensions,
        arrayTypeInst.ArrayType.OwnerId,
        arrayTypeInst.ArrayType.TenantId,
        arrayTypeInst.ArrayType.NumericTypes.UniqId,
        arrayTypeInst.ArrayType.NumericTypes.MdElementId,
        arrayTypeInst.ArrayType.NumericTypes.Version,
        false
      )

      arrayTypeDef.origDef = arrayTypeInst.ArrayType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(arrayTypeInst.ArrayType.ObjectFormat)
      arrayTypeDef.ObjectFormat(objFmt)
      arrayTypeDef.author = arrayTypeInst.ArrayType.Author
      arrayTypeDef.description = arrayTypeInst.ArrayType.Description
      arrayTypeDef.tranId = arrayTypeInst.ArrayType.NumericTypes.TransId
      arrayTypeDef.creationTime = arrayTypeInst.ArrayType.NumericTypes.CreationTime
      arrayTypeDef.modTime = arrayTypeInst.ArrayType.NumericTypes.ModTime
      arrayTypeDef.mdElemStructVer = arrayTypeInst.ArrayType.NumericTypes.MdElemStructVer
      arrayTypeDef.active = arrayTypeInst.ArrayType.IsActive
      arrayTypeDef.deleted = arrayTypeInst.ArrayType.IsDeleted
      arrayTypeDef.implementationName(arrayTypeInst.ArrayType.Implementation)
      arrayTypeDef.dependencyJarNames = arrayTypeInst.ArrayType.DependencyJars.toArray
      arrayTypeDef.jarName = arrayTypeInst.ArrayType.JarName
      arrayTypeDef.PhysicalName(arrayTypeInst.ArrayType.PhysicalName)
      arrayTypeDef.ObjectDefinition(arrayTypeInst.ArrayType.ObjectDefinition)

      arrayTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }
/*
  private def parseArrayBufTypeDef(arrayTypeDefJson: JValue): ArrayBufTypeDef = {
    try {

      logger.debug("Parsed the json : " + arrayTypeDefJson)

      val arrayBufTypeInst = arrayTypeDefJson.extract[ArrayBufType]

      val arrayBufTypeDef = MdMgr.GetMdMgr.MakeArrayBuffer(
        arrayBufTypeInst.ArrayBufType.NameSpace,
        arrayBufTypeInst.ArrayBufType.Name,
        arrayBufTypeInst.ArrayBufType.TypeNameSpace,
        arrayBufTypeInst.ArrayBufType.TypeName,
        arrayBufTypeInst.ArrayBufType.NumberOfDimensions,
        arrayBufTypeInst.ArrayBufType.OwnerId,
        arrayBufTypeInst.ArrayBufType.TenantId,
        arrayBufTypeInst.ArrayBufType.NumericTypes.UniqId,
        arrayBufTypeInst.ArrayBufType.NumericTypes.MdElementId,
        arrayBufTypeInst.ArrayBufType.NumericTypes.Version,
        false
      )
      arrayBufTypeDef.origDef = arrayBufTypeInst.ArrayBufType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(arrayBufTypeInst.ArrayBufType.ObjectFormat)
      arrayBufTypeDef.ObjectFormat(objFmt)
      arrayBufTypeDef.author = arrayBufTypeInst.ArrayBufType.Author
      arrayBufTypeDef.description = arrayBufTypeInst.ArrayBufType.Description
      arrayBufTypeDef.tranId = arrayBufTypeInst.ArrayBufType.NumericTypes.TransId
      arrayBufTypeDef.creationTime = arrayBufTypeInst.ArrayBufType.NumericTypes.CreationTime
      arrayBufTypeDef.modTime = arrayBufTypeInst.ArrayBufType.NumericTypes.ModTime
      arrayBufTypeDef.mdElemStructVer = arrayBufTypeInst.ArrayBufType.NumericTypes.MdElemStructVer
      arrayBufTypeDef.active = arrayBufTypeInst.ArrayBufType.IsActive
      arrayBufTypeDef.deleted = arrayBufTypeInst.ArrayBufType.IsDeleted
      arrayBufTypeDef.implementationName(arrayBufTypeInst.ArrayBufType.Implementation)
      arrayBufTypeDef.dependencyJarNames = arrayBufTypeInst.ArrayBufType.DependencyJars.toArray
      arrayBufTypeDef.jarName = arrayBufTypeInst.ArrayBufType.JarName
      arrayBufTypeDef.PhysicalName(arrayBufTypeInst.ArrayBufType.PhysicalName)
      arrayBufTypeDef.ObjectDefinition(arrayBufTypeInst.ArrayBufType.ObjectDefinition)

      arrayBufTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseSetTypeDef(setTypeDefJson: JValue): SetTypeDef = {
    try {

      logger.debug("Parsed the json : " + setTypeDefJson)

      val setTypeInst = setTypeDefJson.extract[SetType]

      val setTypeDef = MdMgr.GetMdMgr.MakeSet(
        setTypeInst.SetType.NameSpace,
        setTypeInst.SetType.Name,
        setTypeInst.SetType.TypeNameSpace,
        setTypeInst.SetType.TypeName,
        setTypeInst.SetType.NumericTypes.Version,
        setTypeInst.SetType.OwnerId,
        setTypeInst.SetType.TenantId,
        setTypeInst.SetType.NumericTypes.UniqId,
        setTypeInst.SetType.NumericTypes.MdElementId,
        false
      )
      setTypeDef.origDef = setTypeInst.SetType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(setTypeInst.SetType.ObjectFormat)
      setTypeDef.ObjectFormat(objFmt)
      setTypeDef.author = setTypeInst.SetType.Author
      setTypeDef.description = setTypeInst.SetType.Description
      setTypeDef.tranId = setTypeInst.SetType.NumericTypes.TransId
      setTypeDef.creationTime = setTypeInst.SetType.NumericTypes.CreationTime
      setTypeDef.modTime = setTypeInst.SetType.NumericTypes.ModTime
      setTypeDef.mdElemStructVer = setTypeInst.SetType.NumericTypes.MdElemStructVer
      setTypeDef.active = setTypeInst.SetType.IsActive
      setTypeDef.deleted = setTypeInst.SetType.IsDeleted
      setTypeDef.implementationName(setTypeInst.SetType.Implementation)
      setTypeDef.dependencyJarNames = setTypeInst.SetType.DependencyJars.toArray
      setTypeDef.jarName = setTypeInst.SetType.JarName
      setTypeDef.PhysicalName(setTypeInst.SetType.PhysicalName)
      setTypeDef.ObjectDefinition(setTypeInst.SetType.ObjectDefinition)
      setTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseImmutableSetTypeDef(immutableSetTypeDefJson: JValue): ImmutableSetTypeDef = {
    try {

      logger.debug("Parsed the json : " + immutableSetTypeDefJson)

      val immutableSetTypeInst = immutableSetTypeDefJson.extract[ImmutableSetType]

      val immutableSetTypeDef = MdMgr.GetMdMgr.MakeImmutableSet(
        immutableSetTypeInst.ImmutableSetType.NameSpace,
        immutableSetTypeInst.ImmutableSetType.Name,
        immutableSetTypeInst.ImmutableSetType.TypeNameSpace,
        immutableSetTypeInst.ImmutableSetType.TypeName,
        immutableSetTypeInst.ImmutableSetType.NumericTypes.Version,
        immutableSetTypeInst.ImmutableSetType.OwnerId,
        immutableSetTypeInst.ImmutableSetType.TenantId,
        immutableSetTypeInst.ImmutableSetType.NumericTypes.UniqId,
        immutableSetTypeInst.ImmutableSetType.NumericTypes.MdElementId
      )
      immutableSetTypeDef.origDef = immutableSetTypeInst.ImmutableSetType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(immutableSetTypeInst.ImmutableSetType.ObjectFormat)
      immutableSetTypeDef.ObjectFormat(objFmt)
      immutableSetTypeDef.author = immutableSetTypeInst.ImmutableSetType.Author
      immutableSetTypeDef.description = immutableSetTypeInst.ImmutableSetType.Description
      immutableSetTypeDef.tranId = immutableSetTypeInst.ImmutableSetType.NumericTypes.TransId
      immutableSetTypeDef.creationTime = immutableSetTypeInst.ImmutableSetType.NumericTypes.CreationTime
      immutableSetTypeDef.modTime = immutableSetTypeInst.ImmutableSetType.NumericTypes.ModTime
      immutableSetTypeDef.mdElemStructVer = immutableSetTypeInst.ImmutableSetType.NumericTypes.MdElemStructVer
      immutableSetTypeDef.active = immutableSetTypeInst.ImmutableSetType.IsActive
      immutableSetTypeDef.deleted = immutableSetTypeInst.ImmutableSetType.IsDeleted
      immutableSetTypeDef.implementationName(immutableSetTypeInst.ImmutableSetType.Implementation)
      immutableSetTypeDef.dependencyJarNames = immutableSetTypeInst.ImmutableSetType.DependencyJars.toArray
      immutableSetTypeDef.jarName = immutableSetTypeInst.ImmutableSetType.JarName
      immutableSetTypeDef.PhysicalName(immutableSetTypeInst.ImmutableSetType.PhysicalName)
      immutableSetTypeDef.ObjectDefinition(immutableSetTypeInst.ImmutableSetType.ObjectDefinition)
      immutableSetTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseTreeSetTypeDef(treeSetTypeDefJson: JValue): TreeSetTypeDef = {
    try {

      logger.debug("Parsed the json : " + treeSetTypeDefJson)

      val treeSetTypeInst = treeSetTypeDefJson.extract[TreeSetType]

      val arrayBufTypeDef = MdMgr.GetMdMgr.MakeTreeSet(
        treeSetTypeInst.TreeSetType.NameSpace,
        treeSetTypeInst.TreeSetType.Name,
        treeSetTypeInst.TreeSetType.TypeNameSpace,
        treeSetTypeInst.TreeSetType.TypeName,
        treeSetTypeInst.TreeSetType.NumericTypes.Version,
        treeSetTypeInst.TreeSetType.OwnerId,
        treeSetTypeInst.TreeSetType.TenantId,
        treeSetTypeInst.TreeSetType.NumericTypes.UniqId,
        treeSetTypeInst.TreeSetType.NumericTypes.MdElementId,
        false
      )
      arrayBufTypeDef.origDef = treeSetTypeInst.TreeSetType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(treeSetTypeInst.TreeSetType.ObjectFormat)
      arrayBufTypeDef.ObjectFormat(objFmt)
      arrayBufTypeDef.author = treeSetTypeInst.TreeSetType.Author
      arrayBufTypeDef.description = treeSetTypeInst.TreeSetType.Description
      arrayBufTypeDef.tranId = treeSetTypeInst.TreeSetType.NumericTypes.TransId
      arrayBufTypeDef.creationTime = treeSetTypeInst.TreeSetType.NumericTypes.CreationTime
      arrayBufTypeDef.modTime = treeSetTypeInst.TreeSetType.NumericTypes.ModTime
      arrayBufTypeDef.mdElemStructVer = treeSetTypeInst.TreeSetType.NumericTypes.MdElemStructVer
      arrayBufTypeDef.active = treeSetTypeInst.TreeSetType.IsActive
      arrayBufTypeDef.deleted = treeSetTypeInst.TreeSetType.IsDeleted
      arrayBufTypeDef.implementationName(treeSetTypeInst.TreeSetType.Implementation)
      arrayBufTypeDef.dependencyJarNames = treeSetTypeInst.TreeSetType.DependencyJars.toArray
      arrayBufTypeDef.jarName = treeSetTypeInst.TreeSetType.JarName
      arrayBufTypeDef.PhysicalName(treeSetTypeInst.TreeSetType.PhysicalName)
      arrayBufTypeDef.ObjectDefinition(treeSetTypeInst.TreeSetType.ObjectDefinition)

      arrayBufTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseSortedSetTypeDef(sortedSetTypeDefJson: JValue): SortedSetTypeDef = {
    try {
      logger.debug("Parsed the json : " + sortedSetTypeDefJson)

      val sortedSetTypeInst = sortedSetTypeDefJson.extract[SortedSetType]

      val arrayBufTypeDef = MdMgr.GetMdMgr.MakeSortedSet(
        sortedSetTypeInst.SortedSetType.NameSpace,
        sortedSetTypeInst.SortedSetType.Name,
        sortedSetTypeInst.SortedSetType.TypeNameSpace,
        sortedSetTypeInst.SortedSetType.TypeName,
        sortedSetTypeInst.SortedSetType.NumericTypes.Version,
        sortedSetTypeInst.SortedSetType.OwnerId,
        sortedSetTypeInst.SortedSetType.TenantId,
        sortedSetTypeInst.SortedSetType.NumericTypes.UniqId,
        sortedSetTypeInst.SortedSetType.NumericTypes.MdElementId,
        false
      )
      arrayBufTypeDef.origDef = sortedSetTypeInst.SortedSetType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(sortedSetTypeInst.SortedSetType.ObjectFormat)
      arrayBufTypeDef.ObjectFormat(objFmt)
      arrayBufTypeDef.author = sortedSetTypeInst.SortedSetType.Author
      arrayBufTypeDef.description = sortedSetTypeInst.SortedSetType.Description
      arrayBufTypeDef.tranId = sortedSetTypeInst.SortedSetType.NumericTypes.TransId
      arrayBufTypeDef.creationTime = sortedSetTypeInst.SortedSetType.NumericTypes.CreationTime
      arrayBufTypeDef.modTime = sortedSetTypeInst.SortedSetType.NumericTypes.ModTime
      arrayBufTypeDef.mdElemStructVer = sortedSetTypeInst.SortedSetType.NumericTypes.MdElemStructVer
      arrayBufTypeDef.active = sortedSetTypeInst.SortedSetType.IsActive
      arrayBufTypeDef.deleted = sortedSetTypeInst.SortedSetType.IsDeleted
      arrayBufTypeDef.implementationName(sortedSetTypeInst.SortedSetType.Implementation)
      arrayBufTypeDef.dependencyJarNames = sortedSetTypeInst.SortedSetType.DependencyJars.toArray
      arrayBufTypeDef.jarName = sortedSetTypeInst.SortedSetType.JarName
      arrayBufTypeDef.PhysicalName(sortedSetTypeInst.SortedSetType.PhysicalName)
      arrayBufTypeDef.ObjectDefinition(sortedSetTypeInst.SortedSetType.ObjectDefinition)

      arrayBufTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseImmutableMapTypeDef(immutableMapTypeDefJson: JValue): ImmutableMapTypeDef = {
    try {

      logger.debug("Parsed the json : " + immutableMapTypeDefJson)

      val immutableMapTypeInst = immutableMapTypeDefJson.extract[ImmutableMapType]

      val key = (immutableMapTypeInst.ImmutableMapType.KeyTypeNameSpace, immutableMapTypeInst.ImmutableMapType.KeyTypeName)
      val value = (immutableMapTypeInst.ImmutableMapType.ValueTypeNameSpace, immutableMapTypeInst.ImmutableMapType.ValueTypeName)

      val mapTypeDef = MdMgr.GetMdMgr.MakeImmutableMap(immutableMapTypeInst.ImmutableMapType.NameSpace,
        immutableMapTypeInst.ImmutableMapType.Name,
        key,
        value,
        immutableMapTypeInst.ImmutableMapType.NumericTypes.Version,
        immutableMapTypeInst.ImmutableMapType.OwnerId,
        immutableMapTypeInst.ImmutableMapType.TenantId,
        immutableMapTypeInst.ImmutableMapType.NumericTypes.UniqId,
        immutableMapTypeInst.ImmutableMapType.NumericTypes.MdElementId,
        false
      )

      mapTypeDef.origDef = immutableMapTypeInst.ImmutableMapType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(immutableMapTypeInst.ImmutableMapType.ObjectFormat)
      mapTypeDef.ObjectFormat(objFmt)
      mapTypeDef.author = immutableMapTypeInst.ImmutableMapType.Author
      mapTypeDef.description = immutableMapTypeInst.ImmutableMapType.Description
      mapTypeDef.tranId = immutableMapTypeInst.ImmutableMapType.NumericTypes.TransId
      mapTypeDef.creationTime = immutableMapTypeInst.ImmutableMapType.NumericTypes.CreationTime
      mapTypeDef.modTime = immutableMapTypeInst.ImmutableMapType.NumericTypes.ModTime
      mapTypeDef.mdElemStructVer = immutableMapTypeInst.ImmutableMapType.NumericTypes.MdElemStructVer
      mapTypeDef.active = immutableMapTypeInst.ImmutableMapType.IsActive
      mapTypeDef.deleted = immutableMapTypeInst.ImmutableMapType.IsDeleted
      mapTypeDef.implementationName(immutableMapTypeInst.ImmutableMapType.Implementation)
      mapTypeDef.dependencyJarNames = immutableMapTypeInst.ImmutableMapType.DependencyJars.toArray
      mapTypeDef.jarName = immutableMapTypeInst.ImmutableMapType.JarName
      mapTypeDef.PhysicalName(immutableMapTypeInst.ImmutableMapType.PhysicalName)
      mapTypeDef.ObjectDefinition(immutableMapTypeInst.ImmutableMapType.ObjectDefinition)

      mapTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseHashMapTypeDef(hashMapTypeDefJson: JValue): HashMapTypeDef = {
    try {
      logger.debug("Parsed the json : " + hashMapTypeDefJson)

      val hashMapTypeInst = hashMapTypeDefJson.extract[HashMapType]

      val key = (hashMapTypeInst.HashMapType.KeyTypeNameSpace, hashMapTypeInst.HashMapType.KeyTypeName)
      val value = (hashMapTypeInst.HashMapType.ValueTypeNameSpace, hashMapTypeInst.HashMapType.ValueTypeName)

      val hashMapTypeDef = MdMgr.GetMdMgr.MakeHashMap(
        hashMapTypeInst.HashMapType.NameSpace,
        hashMapTypeInst.HashMapType.Name,
        key,
        value,
        hashMapTypeInst.HashMapType.NumericTypes.Version,
        hashMapTypeInst.HashMapType.OwnerId,
        hashMapTypeInst.HashMapType.TenantId,
        hashMapTypeInst.HashMapType.NumericTypes.UniqId,
        hashMapTypeInst.HashMapType.NumericTypes.MdElementId
      )

      hashMapTypeDef.origDef = hashMapTypeInst.HashMapType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(hashMapTypeInst.HashMapType.ObjectFormat)
      hashMapTypeDef.ObjectFormat(objFmt)
      hashMapTypeDef.author = hashMapTypeInst.HashMapType.Author
      hashMapTypeDef.description = hashMapTypeInst.HashMapType.Description
      hashMapTypeDef.tranId = hashMapTypeInst.HashMapType.NumericTypes.TransId
      hashMapTypeDef.creationTime = hashMapTypeInst.HashMapType.NumericTypes.CreationTime
      hashMapTypeDef.modTime = hashMapTypeInst.HashMapType.NumericTypes.ModTime
      hashMapTypeDef.mdElemStructVer = hashMapTypeInst.HashMapType.NumericTypes.MdElemStructVer
      hashMapTypeDef.active = hashMapTypeInst.HashMapType.IsActive
      hashMapTypeDef.deleted = hashMapTypeInst.HashMapType.IsDeleted
      hashMapTypeDef.implementationName(hashMapTypeInst.HashMapType.Implementation)
      hashMapTypeDef.dependencyJarNames = hashMapTypeInst.HashMapType.DependencyJars.toArray
      hashMapTypeDef.jarName = hashMapTypeInst.HashMapType.JarName
      hashMapTypeDef.PhysicalName(hashMapTypeInst.HashMapType.PhysicalName)
      hashMapTypeDef.ObjectDefinition(hashMapTypeInst.HashMapType.ObjectDefinition)

      hashMapTypeDef

    }

    catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseListTypeDef(listTypeDefJson: JValue): ListTypeDef = {
    try {

      logger.debug("Parsed the json : " + listTypeDefJson)

      val listTypeInst = listTypeDefJson.extract[ListType]

      val listTypeDef = MdMgr.GetMdMgr.MakeList(
        listTypeInst.ListType.NameSpace,
        listTypeInst.ListType.Name,
        listTypeInst.ListType.TypeNameSpace,
        listTypeInst.ListType.TypeName,
        listTypeInst.ListType.NumericTypes.Version,
        listTypeInst.ListType.OwnerId,
        listTypeInst.ListType.TenantId,
        listTypeInst.ListType.NumericTypes.UniqId,
        listTypeInst.ListType.NumericTypes.MdElementId
      )
      listTypeDef.origDef = listTypeInst.ListType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(listTypeInst.ListType.ObjectFormat)
      listTypeDef.ObjectFormat(objFmt)
      listTypeDef.author = listTypeInst.ListType.Author
      listTypeDef.description = listTypeInst.ListType.Description
      listTypeDef.tranId = listTypeInst.ListType.NumericTypes.TransId
      listTypeDef.creationTime = listTypeInst.ListType.NumericTypes.CreationTime
      listTypeDef.modTime = listTypeInst.ListType.NumericTypes.ModTime
      listTypeDef.mdElemStructVer = listTypeInst.ListType.NumericTypes.MdElemStructVer
      listTypeDef.active = listTypeInst.ListType.IsActive
      listTypeDef.deleted = listTypeInst.ListType.IsDeleted
      listTypeDef.implementationName(listTypeInst.ListType.Implementation)
      listTypeDef.dependencyJarNames = listTypeInst.ListType.DependencyJars.toArray
      listTypeDef.jarName = listTypeInst.ListType.JarName
      listTypeDef.PhysicalName(listTypeInst.ListType.PhysicalName)
      listTypeDef.ObjectDefinition(listTypeInst.ListType.ObjectDefinition)

      listTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseQueueTypeDef(queueTypeDefJson: JValue): QueueTypeDef = {
    try {

      logger.debug("Parsed the json : " + queueTypeDefJson)

      val queueTypeInst = queueTypeDefJson.extract[QueueType]

      val queueTypeDef = MdMgr.GetMdMgr.MakeQueue(
        queueTypeInst.QueueType.NameSpace,
        queueTypeInst.QueueType.Name,
        queueTypeInst.QueueType.TypeNameSpace,
        queueTypeInst.QueueType.TypeName,
        queueTypeInst.QueueType.NumericTypes.Version,
        queueTypeInst.QueueType.OwnerId,
        queueTypeInst.QueueType.TenantId,
        queueTypeInst.QueueType.NumericTypes.UniqId,
        queueTypeInst.QueueType.NumericTypes.MdElementId
      )
      queueTypeDef.origDef = queueTypeInst.QueueType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(queueTypeInst.QueueType.ObjectFormat)
      queueTypeDef.ObjectFormat(objFmt)
      queueTypeDef.author = queueTypeInst.QueueType.Author
      queueTypeDef.description = queueTypeInst.QueueType.Description
      queueTypeDef.tranId = queueTypeInst.QueueType.NumericTypes.TransId
      queueTypeDef.creationTime = queueTypeInst.QueueType.NumericTypes.CreationTime
      queueTypeDef.modTime = queueTypeInst.QueueType.NumericTypes.ModTime
      queueTypeDef.mdElemStructVer = queueTypeInst.QueueType.NumericTypes.MdElemStructVer
      queueTypeDef.active = queueTypeInst.QueueType.IsActive
      queueTypeDef.deleted = queueTypeInst.QueueType.IsDeleted
      queueTypeDef.implementationName(queueTypeInst.QueueType.Implementation)
      queueTypeDef.dependencyJarNames = queueTypeInst.QueueType.DependencyJars.toArray
      queueTypeDef.jarName = queueTypeInst.QueueType.JarName
      queueTypeDef.PhysicalName(queueTypeInst.QueueType.PhysicalName)
      queueTypeDef.ObjectDefinition(queueTypeInst.QueueType.ObjectDefinition)

      queueTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseTupleTypeDef(tupleTypeDefJson: JValue): TupleTypeDef = {
    try {

      logger.debug("Parsed the json : " + tupleTypeDefJson)

      val tupleTypeInst = tupleTypeDefJson.extract[TupleType]

      val tuples = tupleTypeInst.TupleType.TupleInfo.toArray.map(m => (m.TypeNameSpace, m.TypeName))

      val tupleTypeDef = MdMgr.GetMdMgr.MakeTupleType(
        tupleTypeInst.TupleType.NameSpace,
        tupleTypeInst.TupleType.Name,
        tuples,
        tupleTypeInst.TupleType.NumericTypes.Version,
        tupleTypeInst.TupleType.OwnerId,
        tupleTypeInst.TupleType.TenantId,
        tupleTypeInst.TupleType.NumericTypes.UniqId,
        tupleTypeInst.TupleType.NumericTypes.MdElementId
      )
      tupleTypeDef.origDef = tupleTypeInst.TupleType.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(tupleTypeInst.TupleType.ObjectFormat)
      tupleTypeDef.ObjectFormat(objFmt)
      tupleTypeDef.author = tupleTypeInst.TupleType.Author
      tupleTypeDef.description = tupleTypeInst.TupleType.Description
      tupleTypeDef.tranId = tupleTypeInst.TupleType.NumericTypes.TransId
      tupleTypeDef.creationTime = tupleTypeInst.TupleType.NumericTypes.CreationTime
      tupleTypeDef.modTime = tupleTypeInst.TupleType.NumericTypes.ModTime
      tupleTypeDef.mdElemStructVer = tupleTypeInst.TupleType.NumericTypes.MdElemStructVer
      tupleTypeDef.active = tupleTypeInst.TupleType.IsActive
      tupleTypeDef.deleted = tupleTypeInst.TupleType.IsDeleted
      tupleTypeDef.implementationName(tupleTypeInst.TupleType.Implementation)
      tupleTypeDef.dependencyJarNames = tupleTypeInst.TupleType.DependencyJars.toArray
      tupleTypeDef.jarName = tupleTypeInst.TupleType.JarName
      tupleTypeDef.PhysicalName(tupleTypeInst.TupleType.PhysicalName)
      tupleTypeDef.ObjectDefinition(tupleTypeInst.TupleType.ObjectDefinition)

      tupleTypeDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

*/
  private def parseJarDef(jarDefJson: JValue): JarDef = {
    try {
      logger.debug("Parsed the json : " + jarDefJson)

      val jarInst = jarDefJson.extract[Jar]

      val jarDef = MdMgr.GetMdMgr.MakeJarDef(jarInst.Jar.NameSpace,
        jarInst.Jar.Name,
        jarInst.Jar.NumericTypes.Version.toString,
        jarInst.Jar.OwnerId,
        jarInst.Jar.TenantId,
        jarInst.Jar.NumericTypes.UniqId,
        jarInst.Jar.NumericTypes.MdElementId
      )

      jarDef.origDef = jarInst.Jar.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(jarInst.Jar.ObjectFormat)
      jarDef.ObjectFormat(objFmt)
      jarDef.author = jarInst.Jar.Author
      jarDef.description = jarInst.Jar.Description
      jarDef.tranId = jarInst.Jar.NumericTypes.TransId
      jarDef.creationTime = jarInst.Jar.NumericTypes.CreationTime
      jarDef.modTime = jarInst.Jar.NumericTypes.ModTime
      jarDef.mdElemStructVer = jarInst.Jar.NumericTypes.MdElemStructVer
      jarDef.active = jarInst.Jar.IsActive
      jarDef.deleted = jarInst.Jar.IsDeleted
      jarDef.dependencyJarNames = jarInst.Jar.DependencyJars.toArray
      jarDef.jarName = jarInst.Jar.JarName
      jarDef.PhysicalName(jarInst.Jar.PhysicalName)
      jarDef.ObjectDefinition(jarInst.Jar.ObjectDefinition)

      jarDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw JarDefParsingException(e.getMessage(), e)
      }
    }
  }

  private def parseConfigDef(configDefJson: JValue): ConfigDef = {
    try {
      logger.debug("Parsed the json : " + configDefJson)

      val configInst = configDefJson.extract[Config]

      val configDef = new ConfigDef

      configDef.nameSpace = configInst.Config.NameSpace;
      configDef.name = configInst.Config.Name;
      configDef.ver = configInst.Config.NumericTypes.Version;
      configDef.origDef = configInst.Config.OrigDef
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(configInst.Config.ObjectFormat)
      configDef.ObjectFormat(objFmt)
      configDef.author = configInst.Config.Author
      configDef.description = configInst.Config.Description
      configDef.tranId = configInst.Config.NumericTypes.TransId
      configDef.uniqueId = configInst.Config.NumericTypes.UniqId
      configDef.creationTime = configInst.Config.NumericTypes.CreationTime
      configDef.modTime = configInst.Config.NumericTypes.ModTime
      configDef.mdElemStructVer = configInst.Config.NumericTypes.MdElemStructVer
      configDef.active = configInst.Config.IsActive
      configDef.deleted = configInst.Config.IsDeleted
      configDef.ownerId = configInst.Config.OwnerId
      configDef.tenantId = configInst.Config.TenantId
      configDef.mdElementId = configInst.Config.NumericTypes.MdElementId
      configDef.dependencyJarNames = configInst.Config.DependencyJars.toArray
      configDef.jarName = configInst.Config.JarName
      configDef.contents = configInst.Config.Contents
      configDef.PhysicalName(configInst.Config.PhysicalName)
      configDef.ObjectDefinition(configInst.Config.ObjectDefinition)

      configDef

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + configDefJson, e)
        throw e
      }
    }
  }

  private def parseNodeInfo(nodeInfoJson: JValue): NodeInfo = {
    try {
      logger.debug("Parsed the json : " + nodeInfoJson)
      val nodeInst = nodeInfoJson.extract[Node]

      val nodeInfo = MdMgr.GetMdMgr.MakeNode(
        nodeInst.Node.NodeId,
        nodeInst.Node.NodePort,
        nodeInst.Node.NodeIpAddr,
        nodeInst.Node.JarPaths,
        nodeInst.Node.Scala_home,
        nodeInst.Node.Java_home,
        nodeInst.Node.Classpath,
        nodeInst.Node.ClusterId,
        nodeInst.Node.Power,
        nodeInst.Node.Roles.toArray,
        nodeInst.Node.Description)

      nodeInfo

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + nodeInfoJson, e)
        throw e
      }
    }
  }

  private def parseClusterInfo(clusterInfoJson: JValue): ClusterInfo = {
    try {
      logger.debug("Parsed the json : " + clusterInfoJson)

      val clusterInst = clusterInfoJson.extract[Cluster]

      val clusterInfo = MdMgr.GetMdMgr.MakeCluster(
        clusterInst.Cluster.ClusterId,
        clusterInst.Cluster.Description,
        clusterInst.Cluster.Privileges
      )
      clusterInfo

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + clusterInfoJson, e)
        throw e
      }
    }
  }

  private def parseClusterCfgInfo(clusterCfgInfoJson: JValue): ClusterCfgInfo = {
    try {
      logger.debug("Parsed the json : " + clusterCfgInfoJson)

      val clusterCfgInst = clusterCfgInfoJson.extract[ClusterCfg]

      val cfgMap = new scala.collection.mutable.HashMap[String, String]
      for (m <- clusterCfgInst.ClusterCfg.CfgMap) {
        cfgMap.put(m.Key, m.Value)
      }

      val clusterCfgInfo = MdMgr.GetMdMgr.MakeClusterCfg(
        clusterCfgInst.ClusterCfg.ClusterId,
        cfgMap,
        new Date(clusterCfgInst.ClusterCfg.ModifiedTime),
        new Date(clusterCfgInst.ClusterCfg.CreatedTime)
      )
      val usrConfigs = new scala.collection.mutable.HashMap[String, String]

      for (m <- clusterCfgInst.ClusterCfg.UsrConfigs) {
        usrConfigs.put(m.Key, m.Value)
      }

      clusterCfgInfo.usrConfigs = usrConfigs

      clusterCfgInfo

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + clusterCfgInfoJson, e)
        throw e
      }
    }
  }

  private def parseTenantInfo(tenantInfoJson: JValue): TenantInfo = {
    try {
      logger.debug("Parsed the json : " + tenantInfoJson)

      val tenantInst = tenantInfoJson.extract[Tenant]

      val tenantInfo = MdMgr.GetMdMgr.MakeTenantInfo(
        tenantInst.Tenant.TenantId,
        tenantInst.Tenant.Description,
        tenantInst.Tenant.PrimaryDataStore,
        tenantInst.Tenant.CacheConfig
      )
      tenantInfo
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + tenantInfoJson, e)
        throw e
      }
    }
  }

  private def parseAdapterInfo(adapterInfoJson: JValue): AdapterInfo = {
    try {
      logger.debug("Parsed the json : " + adapterInfoJson)

      val adapterInst = adapterInfoJson.extract[Adapter]

      val adapterInfo = MdMgr.GetMdMgr.MakeAdapter(
        adapterInst.Adapter.Name,
        adapterInst.Adapter.TypeString,
        adapterInst.Adapter.ClassName,
        adapterInst.Adapter.JarName,
        adapterInst.Adapter.DependencyJars,
        adapterInst.Adapter.AdapterSpecificCfg,
        adapterInst.Adapter.TenantId,
        adapterInst.Adapter.FullAdapterConfig
      )
      adapterInfo

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + adapterInfoJson, e)
        throw e
      }
    }
  }

  private def parseUserPropertiesInfo(userPropertiesInfoJson: JValue): UserPropertiesInfo = {
    try {
      logger.debug("Parsed the json : " + userPropertiesInfoJson)

      val userPropertiesInst = userPropertiesInfoJson.extract[UserProperties]

      val props = new scala.collection.mutable.HashMap[String, String]
      for (m <- userPropertiesInst.UserProperties.Props) {
        props.put(m.Key, m.Value)
      }

      val upi = new UserPropertiesInfo
      upi.clusterId = userPropertiesInst.UserProperties.ClusterId
      upi.props = props
      upi

    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to parse JSON" + userPropertiesInfoJson, e)
        throw e
      }
    }
  }

  private def getEmptyIfNull(jarName: String): String = {
    if (jarName != null) jarName else ""
  }

  private def getEmptyArrayIfNull(arr: Array[String]): Array[String] = {
    if (arr != null)
      arr
    else
      Array.empty[String]
  }

  private def getEmptyHashMapIfNull(map: scala.collection.mutable.HashMap[String, String]): scala.collection.mutable.HashMap[String, String] = {
    if (map != null)
      map
    else
      new scala.collection.mutable.HashMap[String, String]
  }

  private def getZeroIfDateIsNull(dt: Date): Long = {
    if (dt != null)
      dt.getTime
    else
      0L

  }

}

case class UserPropertiesInformation(ClusterId: String, Props: List[KeyVale])

case class UserProperties(UserProperties: UserPropertiesInformation)

case class AdapterInformation(Name: String, TypeString: String, ClassName: String, JarName: String, DependencyJars: List[String], AdapterSpecificCfg: String, TenantId: String, FullAdapterConfig: String)

case class Adapter(Adapter: AdapterInformation)

case class TenantInformation(TenantId: String, Description: String, PrimaryDataStore: String, CacheConfig: String)

case class Tenant(Tenant: TenantInformation)

case class KeyVale(Key: String, Value: String)

case class ClusterCfgInformation(ClusterId: String, CfgMap: List[KeyVale], ModifiedTime: Long, CreatedTime: Long, UsrConfigs: List[KeyVale])

case class ClusterCfg(ClusterCfg: ClusterCfgInformation)

case class ClusterInformation(ClusterId: String, Description: String, Privileges: String)

case class Cluster(Cluster: ClusterInformation)

case class NodeInformation(NodeId: String, NodePort: Int, NodeIpAddr: String, JarPaths: List[String], Scala_home: String, Java_home: String, Classpath: String, ClusterId: String, Power: Int, Roles: List[String], Description: String)

case class Node(Node: NodeInformation)

case class Argument(ArgName: String, ArgTypeNameSpace: String, ArgTypeName: String)

case class ConfigInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, Description: String, Contents: String, NumericTypes: NumericTypes, TenantId: String)

case class Config(Config: ConfigInfo)

case class JarInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String)

case class Jar(Jar: JarInfo)

case class ArrayTypeInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, TypeTypeName: String, Implementation: String, NumberOfDimensions: Int, TypeName: String, TypeNameSpace: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, IsFixed: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String)

//case class SetTypeInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, TypeTypeName: String, Implementation: String, TypeName: String, TypeNameSpace: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, IsFixed: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String)

//case class SetType(SetType: SetTypeInfo)

//case class ImmutableSetType(ImmutableSetType: SetTypeInfo)

//case class TreeSetType(TreeSetType: SetTypeInfo)

//case class SortedSetType(SortedSetType: SetTypeInfo)

//case class ImmutableMapType(ImmutableMapType: MapTypeInfo)

case class HashMapType(HashMapType: MapTypeInfo)

//case class ListType(ListType: SetTypeInfo)

//case class QueueType(QueueType: SetTypeInfo)

//case class TupleType(TupleType: TupleTypeInfo)

//case class TupleTypeInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, Implementation: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, IsFixed: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String, TupleInfo: List[Tupleinfo])

//case class Tupleinfo(TypeName: String, TypeNameSpace: String)

//case class ArrayBufType(ArrayBufType: ArrayTypeInfo)

case class ArrayType(ArrayType: ArrayTypeInfo)

case class MapTypeInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, TypeTypeName: String, Implementation: String, ValueTypeNameSpace: String, ValueTypeName: String, ObjectDefinition: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, IsFixed: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String)

case class MapType(MapType: MapTypeInfo)

case class FunctionInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, DependencyJars: List[String], OrigDef: String, ObjectFormat: String, Author: String, Arguments: List[Argument], Features: List[String], ReturnTypeNameSpace: String, ObjectDefinition: String, ReturnTypeName: String, ClassName: String, OwnerId: String, IsActive: Boolean, IsDeleted: Boolean, Description: String, NumericTypes: NumericTypes, TenantId: String)

case class Function(Function: FunctionInfo)

case class Attr(NameSpace: String, Name: String, Version: Long, CollectionType: Option[String], Type: TypeDef)

case class MsgAttr(NameSpace: String, Name: String, TypNameSpace: String, TypName: String, Version: Long, CollectionType: String)

case class MessageInfo(NameSpace: String, Name: String, JarName: String, PhysicalName: String, DependencyJars: List[String], MsgAttributes: List[MsgAttr], OrigDef: String, ObjectDefinition: String, ObjectFormat: String, Description: String, OwnerId: String, Author: String, PartitionKey: List[String], Persist: Boolean, IsActive: Boolean, IsDeleted: Boolean, SchemaId: Int, AvroSchema: String, PrimaryKeys: List[PrimaryKeys], ForeignKeys: List[ForeignKeys], NumericTypes: NumericTypes, TenantId: String)

case class PrimaryKeys(constraintName: String, key: List[String])

case class ForeignKeys(constraintName: String, key: List[String], forignContainerName: String, forignKey: List[String])

case class MessageDefin(Message: MessageInfo)

case class ContainerDefin(Container: MessageInfo)

case class ModelDefinition(Model: ModelInfo)

case class MsgAndAttrib(Origin: String, Message: String, Attributes: List[String])

case class ModelInfo(Name: String, PhysicalName: String, JarName: String, NameSpace: String, ObjectFormat: String, BooleanTypes: BooleanTypes, OwnerId: String, OutputMsgs: List[String], ModelType: String, DependencyJars: List[String], ModelRep: String, OrigDef: String, ObjectDefinition: String, NumericTypes: NumericTypes, Description: String, Author: String, ModelConfig: String, inputMsgSets: List[List[MsgAndAttrib]], TenantId: String)

case class NumericTypes(Version: Long, TransId: Long, UniqId: Long, CreationTime: Long, ModTime: Long, MdElemStructVer: Int, MdElementId: Long)

case class BooleanTypes(IsReusable: Boolean, IsActive: Boolean, SupportsInstanceSerialization: Boolean, IsDeleted: Boolean)