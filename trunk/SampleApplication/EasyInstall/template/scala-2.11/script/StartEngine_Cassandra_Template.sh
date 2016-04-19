#!/bin/bash

# Start the engine with cassandra backed metadata configuration.  Cassandra must be running, not to mention the zookeeper and your queue software 
ipport="8998"

if [ "$1" != "debug" ]; then
	java -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j2.xml -cp {InstallDirectory}/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:{InstallDirectory}/lib/system/kamanjamanager_2.11-1.4.0.jar com.ligadata.KamanjaManager.KamanjaManager --config {InstallDirectory}/config/Engine1Config_Cassandra.properties
else
	java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j2.xml -cp {InstallDirectory}/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:{InstallDirectory}/lib/system/kamanjamanager_2.11-1.4.0.jar com.ligadata.KamanjaManager.KamanjaManager --config {InstallDirectory}/config/Engine1Config_Cassandra.properties
fi
