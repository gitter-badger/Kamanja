#!/bin/bash

# Install the cluster metadata (cassandra).  If the "debug" is supplied as the first parameter, start the MetadataAPI-1.0 with the 
# debugger. As a pre-requisite, make sure your cassandra instance is running.

ipport="8998"

if [ "$1" != "debug" ]; then
	java -Dlog4j.configurationFile=file:{InstallDirectory}/MigrationAndClusterInstall-2.11/config/log4j.properties -jar {InstallDirectory}/MigrationAndClusterInstall-2.11/bin/MetadataAPI-1.0 --config {InstallDirectory}/MigrationAndClusterInstall-2.11/config/ClusterCfgMetadataAPIConfig_Cassandra.properties
else
	java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:{InstallDirectory}/MigrationAndClusterInstall-2.11/config/log4j.properties -jar {InstallDirectory}/MigrationAndClusterInstall-2.11/bin/MetadataAPI-1.0 --config {InstallDirectory}/MigrationAndClusterInstall-2.11/config/ClusterCfgMetadataAPIConfig_Cassandra.properties
fi


