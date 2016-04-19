    val clusterId : String = if (options.contains('clusterId)) options.apply('clusterId) else null
    val apiConfigPath : String = if (options.contains('apiConfig)) options.apply('apiConfig) else null
    val nodeConfigPath : String = if (options.contains('clusterConfig)) options.apply('clusterConfig) else null
    val tarballPath : String = if (options.contains('tarballPath)) options.apply('tarballPath) else null
    val fromKamanja : String = if (options.contains('fromKamanja)) options.apply('fromKamanja) else null
    val fromScala : String = if (options.contains('fromScala)) options.apply('fromScala) else "2.10"
    val toScala : String = if (options.contains('toScala)) options.apply('toScala) else "2.11"
    val workingDir : String = if (options.contains('workingDir)) options.apply('workingDir) else null
    val upgrade : Boolean = if (options.contains('upgrade)) options.apply('upgrade) == "true" else false
    val install : Boolean = if (options.contains('install)) options.apply('install) == "true" else false



export KAMANJA_INSTALL_HOME=/tmp/drdigital/KamanjaInstall-1.3.2_2.11
export KAMANJA_HOME=/tmp/drdigital/Kamanja-1.3.2_2.11
export KAMANJA_SRCDIR=/home/rich/github/dev/1.3.2.Test/kamanja/trunk
export ipport="8998"
cd $KAMANJA_HOME
mkdir -p /tmp/work

*Unknown option*
java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:$LOG4J_HOME/config/log4j2.xml -jar $KAMANJA_INSTALL_HOME/bin/clusterInstallerDriver-1.0  --clusterId "ligadata1" --apiConfig "$KAMANJA_HOME/config/MetadataAPIConfig.properties" --clusterConfig "$KAMANJA_HOME/config/ClusterConfig.json" --tarballPath "/tmp/drdigital.tar.gz" --fromKamanja "1.1" --fromScala "2.10" --toScala "2.11" --workingDir /tmp/work --upgrade  --logDir /tmp/clusterInstallerLogs --migrationTemplate $KAMANJA_SRCDIR/SampleApplication/clusterInstallerDriver/src/main/resources/MigrateConfigTemplate.json --componentVersionScriptAbsolutePath $KAMANJA_SRCDIR/SampleApplication/clusterInstallerDriver/src/main/resources/GetComponentsVersions.sh --componentVersionJarAbsolutePath $KAMANJA_INSTALL_HOME/bin/GetComponent-1.0


*Missing Arguments*
java -jar $KAMANJA_INSTALL_HOME/bin/clusterInstallerDriver-1.0  --clusterId "ligadata1" --apiConfig "$KAMANJA_HOME/config/MetadataAPIConfig.properties" --clusterConfig "$KAMANJA_HOME/config/ClusterConfig.json" --tarballPath "/tmp/drdigital.tar.gz" --fromKamanja "1.1" --fromScala "2.10" --toScala "2.11" --workingDir /tmp/work --upgrade  


java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:$LOG4J_HOME/config/log4j2.xml -jar $KAMANJA_INSTALL_HOME/bin/clusterInstallerDriver-1.0  --clusterId "ligadata1" --apiConfig "$KAMANJA_HOME/config/MetadataAPIConfig.properties" --clusterConfig "$KAMANJA_HOME/config/ClusterConfig.json" --tarballPath "/tmp/drdigital.tar.gz" --fromKamanja "1.1" --fromScala "2.10" --toScala "2.11" --workingDir /tmp/work --upgrade  --logDir /tmp/clusterInstallerLogs --migrateTemplate $KAMANJA_INSTALL_HOME/config/Migrate_Template.json --componentVersionScriptAbsolutePath $KAMANJA_INSTALL_HOME/bin/GetComponentsVersions.sh --componentVersionJarAbsolutePath $KAMANJA_INSTALL_HOME/bin/GetComponent-1.0


/home/rich/github/dev/1.3.2.Test/kamanja/trunk/SampleApplication/clusterInstallerDriver/src/main/resources/GetComponentsVersions.sh  --componentVersionJarAbsolutePath /tmp/drdigital/KamanjaInstall-1.3.2_2.11/bin/GetComponent-1.0 --componentVersionJarFileName GetComponent-1.0 --remoteNodeIp localhost --resultsFileAbsolutePath /tmp/componentResultsFilelocalhost.txt --resultFileName componentResultsFilelocalhost.txt --rootDirPath /tmp/drdigital/Kamanja-1.3.2_2.11 --pathOutputFileName __path_output_1_2116179210_764718688_643382405_255358643 --jsonArg " {[{ "component" : "zookeeper", "hostslist" : "localhost:2181" } , { "component" : "kafka", "hostslist" : "localhost:9092,localhost:9092,localhost:9092,localhost:9092,localhost:9092" } , { "component" : "hbase", "hostslist" : "Map(StoreType -> hashmap, SchemaName -> testdata, Location -> /tmp/drdigital/Kamanja-1.3.2_2.11/storage)" } , { "component" : "scala", "hostslist" : "localhost" } , { "component" : "java", "hostslist" : "localhost" }]}  "


Seq("bash", "-c", "KamanjaClusterInstall.sh  --MetadataAPIConfig $apiConfigPath --NodeConfigPath $nodeConfigPath --ParentPath $parentPath --PriorInstallDirName $priorInstallDirName --NewInstallDirName $newInstallDirName --TarballPath $tarballPath --ipAddrs $ips --ipIdTargPaths $ipIdTargPaths --ipPathPairs $ipPathPairs --priorInstallDirPath $priorInstallDirPath --newInstallDirPath $newInstallDirPath")
        log.emit(s"KamanjaClusterInstall cmd used: $installCmd")