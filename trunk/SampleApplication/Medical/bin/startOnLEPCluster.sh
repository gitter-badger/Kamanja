#!/bin/bash

# startKamanjaCluster.sh
#
#	NOTE: This script must currently be run from a trunk directory that contains the InstallScript project:

#	example: startKamanjaCluster.sh --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties --NodeConfigPath SampleApplication/Medical/Configs/Engine2BoxConfigV1.json 
#

name1=$1
val1=$2
name2=$3
val2=$4

if [[ "$#" -eq 2 || "$#" -eq 4 ]]; then
	echo
else 
    echo "Incorrect number of arguments"
    echo "usage:"
    echo "  $0 --MetadataAPIConfig  <metadataAPICfgPath> [--NodeConfigPath <kamanjaCfgPath> ]"
    exit 1
fi

if [[ "$name1" != "--MetadataAPIConfig" && "$name1" != "--NodeConfigPath" ]]; then
	echo "Bad arguments"
	echo "usage:"
	echo "	$0  --MetadataAPIConfig  <metadataAPICfgPath> [--NodeConfigPath <kamanjaCfgPath> ]"
	exit 1
fi

currDirPath=`pwd`
currDir=`echo $currDirPath | sed 's/.*\/\(.*\)/\1/g'`
#if [ "$currDir" != "trunk" ]; then
#    echo "Currently this script must be run from the trunk directory of a valid local git repo that has had NodeInfoExtract fat jar built."
#    echo "This application extracts the node information from the metadata and/or engine node config supplied here. "
#    echo "currDir = $currDir"
#    echo "usage:"
#    echo "  $0  --MetadataAPIConfig  <metadataAPICfgPath> [--NodeConfigPath <kamanjaCfgPath> ]"
#    exit 1
#fi

# 1) determine which machines and installation directories are to get the build from the metadata and Kamanja config
# A number of files are produced, all in the working dir. For cluster start only the quartet file is used
workDir="/tmp" 
ipFile="ip.txt"
ipPathPairFile="ipPath.txt"
ipIdCfgTargPathQuartetFileName="ipIdCfgTarg.txt"

nodeInfoExtractDir="$currDirPath/Utils/NodeInfoExtract/target/scala-2.11"
echo "...extract node information for the cluster to be installed from the Metadata and optional Kamanja config supplied"
if  [ "$#" -eq 4 ]; then
    echo "...Command = $nodeInfoExtractDir/NodeInfoExtract-1.0 $name1 \"$val1\" $name2 \"$val2\"  --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\""
    "$nodeInfoExtractDir"/NodeInfoExtract-1.0 "$name1" "$val1" "$name2" "$val2" --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName"
else # -eq 2 
    echo "...Command = $nodeInfoExtractDir/NodeInfoExtract-1.0 $name1 \"$val1\" --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\""
        "$nodeInfoExtractDir"/NodeInfoExtract-1.0 "$name1" "$val1" --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName"
fi


# NOTE: echo "/tmp/node2.cfg" | sed 's/.*\/\(.*\)/\1/g' produces "node2.cfg"
# Start the cluster nodes using the information extracted from the metadata and supplied config.
echo "...start the Kamanja cluster "
exec 12<&0 # save current stdin
exec < "$workDir/$ipIdCfgTargPathQuartetFileName"
while read LINE; do
    machine=$LINE
    read LINE
    id=$LINE
    read LINE
    cfgFile=$LINE
    read LINE
    targetPath=$LINE
    echo "quartet = $machine, $id, $cfgFile, $targetPath"
    echo "...On machine $machine, starting KamanjaManager with configuration $cfgFile for nodeId $id to $machine:$targetPath"
    scp "$cfgFile" "$machine:$targetPath/"
	ssh -T $machine  <<-EOF
	        cd $targetPath
	        nodeCfg=`echo $cfgFile | sed 's/.*\/\(.*\)/\1/g'`
	        java -jar ./KamanjaManager-1.0 --config "./$cfgFile" &
EOF
done
exec 0<&12 12<&-

echo

