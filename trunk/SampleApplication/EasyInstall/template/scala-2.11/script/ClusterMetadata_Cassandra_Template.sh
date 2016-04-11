#!/bin/bash

# Install the cluster metadata (cassandra).  If the "debug" is supplied as the first parameter, start the MetadataAPI-1.0 with the 
# debugger. As a pre-requisite, make sure your cassandra instance is running.

ipport="8998"

if [ "$1" != "debug" ]; then
	java -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j.properties -cp {InstallDirectory}/lib/system/jarfactoryofmodelinstancefactory_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:{InstallDirectory}/lib/system/metadataapi_2.11-1.4.0.jar com.ligadata.MetadataAPI.StartMetadataAPI --config {InstallDirectory}/config/ClusterCfgMetadataAPIConfig_Cassandra.properties
else
	java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:{InstallDirectory}/config/log4j.properties -cp {InstallDirectory}/lib/system/jarfactoryofmodelinstancefactory_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:{InstallDirectory}/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:{InstallDirectory}/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:{InstallDirectory}/lib/system/metadataapi_2.11-1.4.0.jar com.ligadata.MetadataAPI.StartMetadataAPI--config {InstallDirectory}/config/ClusterCfgMetadataAPIConfig_Cassandra.properties
fi


