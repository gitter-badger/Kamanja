#!/bin/sh
KAMANJA_HOME={InstallDirectory}

# Start the engine with hashdb backed metadata configuration.  The zookeeper and your queue software should be running
ipport="8998"

if [ "$1" != "debug" ]; then
    java -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j2.xml -cp {InstallDirectory}/lib/system/jarfactoryofmodelinstancefactory_2.10-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs2_2.10-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.10-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.10-1.4.0.jar:{InstallDirectory}/lib/system/kamanjamanager_2.10-1.4.0.jar com.ligadata.KamanjaManager.KamanjaManager --config {InstallDirectory}/config/Engine1Config.properties
else
	java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j2.xml -cp {InstallDirectory}/lib/system/jarfactoryofmodelinstancefactory_2.10-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs2_2.10-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.10-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.10-1.4.0.jar:{InstallDirectory}/lib/system/kamanjamanager_2.10-1.4.0.jar com.ligadata.KamanjaManager.KamanjaManager --config {InstallDirectory}/config/Engine1Config.properties
fi
