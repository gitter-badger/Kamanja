import sbt._
import Keys._

sbtPlugin := true

version := "0.0.0.1"

scalaVersion := "2.10.4"

shellPrompt := { state =>  "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

net.virtualvoid.sbt.graph.Plugin.graphSettings

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

resolvers += Resolver.file("Local repo", file(System.getProperty("user.home") + "/.ivy2/local"))(Resolver.ivyStylePatterns)

val Organization = "com.ligadata"

lazy val BaseTypes = project.in(file("BaseTypes")) dependsOn(Metadata, Exceptions)

lazy val BaseFunctions = project.in(file("BaseFunctions")) dependsOn(Metadata, Exceptions)

lazy val Serialize = project.in(file("Utils/Serialize")) dependsOn(Metadata, AuditAdapterBase, Exceptions)

lazy val ZooKeeperClient = project.in(file("Utils/ZooKeeper/CuratorClient")) dependsOn(Serialize, Exceptions)

lazy val ZooKeeperListener = project.in(file("Utils/ZooKeeper/CuratorListener")) dependsOn(ZooKeeperClient, Serialize, Exceptions)

lazy val Exceptions = project.in(file("Exceptions"))

lazy val KamanjaBase = project.in(file("KamanjaBase")) dependsOn(Metadata, Exceptions, KamanjaUtils)

lazy val KamanjaManager = project.in(file("KamanjaManager")) dependsOn(Metadata, KamanjaBase, KamanjaData, MetadataBootstrap, MetadataAPI, Serialize, ZooKeeperListener, ZooKeeperLeaderLatch, Exceptions, HeartBeat, InputOutputAdapterBase, KamanjaUtils, TransactionService, JpmmlAdapter)

lazy val InputOutputAdapterBase = project.in(file("InputOutputAdapters/InputOutputAdapterBase")) dependsOn(Exceptions)

lazy val IbmMqSimpleInputOutputAdapters = project.in(file("InputOutputAdapters/IbmMqSimpleInputOutputAdapters")) dependsOn(InputOutputAdapterBase, Exceptions)

lazy val KafkaSimpleInputOutputAdapters = project.in(file("InputOutputAdapters/KafkaSimpleInputOutputAdapters")) dependsOn(InputOutputAdapterBase, Exceptions)

lazy val FileSimpleInputOutputAdapters = project.in(file("InputOutputAdapters/FileSimpleInputOutputAdapters")) dependsOn(InputOutputAdapterBase, Exceptions)

lazy val SimpleEnvContextImpl = project.in(file("EnvContexts/SimpleEnvContextImpl")) dependsOn(KamanjaBase, KamanjaData, StorageManager, Serialize, Exceptions)

lazy val StorageBase = project.in(file("Storage/StorageBase")) dependsOn(Exceptions, KamanjaUtils)

lazy val Metadata = project.in(file("Metadata")) dependsOn(Exceptions)

lazy val OutputMsgDef  = project.in(file("OutputMsgDef")) dependsOn(Metadata,KamanjaBase,BaseTypes)

lazy val MessageDef = project.in(file("MessageDef")) dependsOn(Metadata, MetadataBootstrap, Exceptions)

lazy val LoadtestCommon = project.in(file("Tools/LoadtestCommon")) dependsOn(StorageManager, Exceptions)

lazy val LoadtestRunner = project.in(file("Tools/LoadtestRunner")) dependsOn(LoadtestCommon, Exceptions)

lazy val LoadtestMaster = project.in(file("Tools/LoadtestMaster")) dependsOn(LoadtestCommon, Exceptions)

lazy val Loadtest = project.in(file("Tools/Loadtest")) dependsOn(StorageManager, Exceptions)

lazy val PmmlRuntime = project.in(file("Pmml/PmmlRuntime")) dependsOn(Metadata, KamanjaBase, Exceptions) 

lazy val PmmlCompiler = project.in(file("Pmml/PmmlCompiler")) dependsOn(PmmlRuntime, PmmlUdfs, Metadata, KamanjaBase, MetadataBootstrap, Exceptions)

lazy val PmmlUdfs = project.in(file("Pmml/PmmlUdfs")) dependsOn(Metadata, PmmlRuntime, KamanjaBase, Exceptions)

lazy val JpmmlAdapter = project.in(file("Pmml/JpmmlAdapter")) dependsOn(Metadata, KamanjaBase, Exceptions)

lazy val MethodExtractor = project.in(file("Pmml/MethodExtractor")) dependsOn(PmmlUdfs, Metadata, KamanjaBase, Serialize, Exceptions)

lazy val MetadataAPI = project.in(file("MetadataAPI")) dependsOn(StorageManager,Metadata,MessageDef,PmmlCompiler,Serialize,ZooKeeperClient,ZooKeeperListener,OutputMsgDef,Exceptions, SecurityAdapterBase, KamanjaUtils, HeartBeat)

lazy val MetadataBootstrap = project.in(file("MetadataBootstrap/Bootstrap")) dependsOn(Metadata, KamanjaBase, BaseTypes, Exceptions)

lazy val MetadataAPIService = project.in(file("MetadataAPIService")) dependsOn(KamanjaBase,MetadataAPI,ZooKeeperLeaderLatch, Exceptions)

lazy val MetadataAPIServiceClient = project.in(file("MetadataAPIServiceClient")) dependsOn(Serialize, Exceptions, KamanjaBase)

lazy val SimpleKafkaProducer = project.in(file("Utils/SimpleKafkaProducer")) dependsOn(Metadata, KamanjaBase, Exceptions)

lazy val KVInit = project.in(file("Utils/KVInit")) dependsOn (Metadata, KamanjaBase, KamanjaData, MetadataBootstrap, MetadataAPI, StorageManager, Exceptions, TransactionService)

lazy val ZooKeeperLeaderLatch = project.in(file("Utils/ZooKeeper/CuratorLeaderLatch")) dependsOn(ZooKeeperClient, Exceptions)

lazy val JsonDataGen = project.in(file("Utils/JsonDataGen")) dependsOn(Exceptions, KamanjaBase)

lazy val NodeInfoExtract  = project.in(file("Utils/NodeInfoExtract")) dependsOn(MetadataAPI, Exceptions)

lazy val Controller = project.in(file("Utils/Controller")) dependsOn(ZooKeeperClient,ZooKeeperListener,KafkaSimpleInputOutputAdapters, Exceptions)

lazy val SimpleApacheShiroAdapter = project.in(file("Utils/Security/SimpleApacheShiroAdapter")) dependsOn(Metadata, Exceptions, SecurityAdapterBase)

lazy val AuditAdapters = project.in(file("Utils/Audit")) dependsOn(StorageManager, Exceptions, AuditAdapterBase,Serialize)

lazy val KamanjaData = project.in(file("KamanjaData")) dependsOn(KamanjaBase, Exceptions)

lazy val CustomUdfLib = project.in(file("SampleApplication/CustomUdfLib")) dependsOn(PmmlUdfs, Exceptions)

lazy val JdbcDataCollector = project.in(file("Utils/JdbcDataCollector")) dependsOn(Exceptions)

lazy val ExtractData = project.in(file("Utils/ExtractData")) dependsOn(Metadata, KamanjaBase, KamanjaData, MetadataBootstrap, MetadataAPI, StorageManager, Exceptions)

lazy val InterfacesSamples = project.in(file("SampleApplication/InterfacesSamples")) dependsOn(Metadata, KamanjaBase, KamanjaData, MetadataBootstrap, MetadataAPI, StorageBase, Exceptions)

lazy val StorageCassandra = project.in(file("Storage/Cassandra")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageHashMap = project.in(file("Storage/HashMap")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageHBase = project.in(file("Storage/HBase")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageRedis = project.in(file("Storage/Redis")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageTreeMap = project.in(file("Storage/TreeMap")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageVoldemort = project.in(file("Storage/Voldemort")) dependsOn(StorageBase, Exceptions, KamanjaUtils)

lazy val StorageManager = project.in(file("Storage/StorageManager")) dependsOn(StorageBase, Exceptions, StorageCassandra, StorageHashMap, StorageHBase, StorageTreeMap, StorageRedis, KamanjaBase, KamanjaUtils)

lazy val AuditAdapterBase = project.in(file("AuditAdapters/AuditAdapterBase")) dependsOn(Exceptions)

lazy val SecurityAdapterBase = project.in(file("SecurityAdapters/SecurityAdapterBase")) dependsOn(Exceptions)

lazy val KamanjaUtils = project.in(file("KamanjaUtils")) dependsOn(Exceptions)

lazy val UtilityService = project.in(file("Utils/UtilitySerivce")) dependsOn(Exceptions, KamanjaUtils)

lazy val HeartBeat = project.in(file("HeartBeat")) dependsOn(ZooKeeperListener, ZooKeeperLeaderLatch, Exceptions)

lazy val TransactionService = project.in(file("TransactionService")) dependsOn(Exceptions, KamanjaBase, ZooKeeperClient, StorageBase, StorageManager, KamanjaData)
