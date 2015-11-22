#!/bin/bash

# clusterInstallKamanja.sh
#
#	Run this script from the trunk directory that contains the release source:

#	Examples: 
#       clusterInstallKamanja.sh --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties --NodeConfigPath SampleApplication/Medical/Configs/Engine2BoxConfigV1.json 
#       clusterInstallKamanja.sh --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties 
#
#   In the first example, a cluster configuration is presumably presented in the NodeConfigPath file (i.e, the cluster decl
#   is new).  In the second example, the cluster config info is retrieved from the metadata store.
#
#   If a cluster config is not present there when no NodeConfigPath argument is presented, an exception is thrown.  In fact
#   if the cluster map returned by the metadata api is empty, one is thrown in any case.  The other reason is that the 
#   engine cluster config is messed up in the NodeConfigPath file supplied.
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
if [ "$currDir" != "trunk" ]; then
    echo "Currently this script must be run from the trunk directory of a valid local git repo that has had NodeInfoExtract fat jar built."
    echo "This application extracts the node information from the metadata and/or engine node config supplied here. "
    echo "currDir = $currDir"
    echo "usage:"
    echo "  $0  --MetadataAPIConfig  <metadataAPICfgPath> [--NodeConfigPath <kamanjaCfgPath> ]"
    exit 1
fi



# 1 build the installation in the staging directory
workDir="/tmp" 
workDirSansSlash="tmp" #tar will trunc the leading slash when building its archive
dirName="KamanjaClusterInstall" 
stagingDir="$workDir/$dirName"
mkdir -p "$stagingDir"
echo "...build the Kamanja installation directory in $stagingDir"
installKamanja_Medical.sh "$stagingDir" `pwd`


# 2) determine which machines and installation directories are to get the build from the metadata and Kamanja config
# A number of files are produced, all in the working dir.
ipFile="ip.txt"
ipPathPairFile="ipPath.txt"
ipIdCfgTargPathQuartetFileName="ipIdCfgTarg.txt"

trunkDir=`pwd`
nodeInfoExtractDir="$trunkDir/Utils/NodeInfoExtract/target/scala-2.11"
echo "...extract node information for the cluster to be installed from the Metadata and Kamanja config supplied"
if  [ "$#" -eq 4 ]; then
    echo "...Command = $nodeInfoExtractDir/NodeInfoExtract-1.0 $name1 \"$val1\" $name2 \"$val2\"  --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\""
    "$nodeInfoExtractDir"/NodeInfoExtract-1.0 "$name1" "$val1" "$name2" "$val2" --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName"
else # -eq 2 
    echo "...Command = $nodeInfoExtractDir/NodeInfoExtract-1.0 $name1 \"$val1\" --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\""
        "$nodeInfoExtractDir"/NodeInfoExtract-1.0 "$name1" "$val1" --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName"
fi

# 3) compress staging dir and tar it
dtPrefix="Kamanja`date +"%Y%b%d"`"
tarName="$dtPrefix.$dirName.bz2"
echo "...compress and tar the installation directory $stagingDir to $tarName"
tar cjf "$workDir/$tarName" "$stagingDir"

# 4) Push the tarballs to each machine defined in the supplied configuration
echo "...copy the tarball to the machines in this cluster"
exec 12<&0 # save current stdin
exec < "$workDir/$ipFile"
while read LINE; do
    machine=$LINE
    echo "...copying $tarName to $machine"
    ssh $machine "mkdir -p $workDir"
    scp "$workDir/$tarName" "$machine:$workDir/$tarName"
done
exec 0<&12 12<&-

echo

# 5) untar/decompress tarballs there and move them into place
echo "...for each directory specified on each machine participating in the cluster, untar and decompress the software to $workDir/$workDirSansSlash/$dirName... then move to corresponding target path"
exec 12<&0 # save current stdin
exec < "$workDir/$ipPathPairFile"
while read LINE; do
    machine=$LINE
    read LINE
    targetPath=$LINE
	ssh -T $machine  <<-EOF
	        cd $workDir
	        rm -Rf $workDirSansSlash
	        tar xjf $tarName
	        rm -Rf $targetPath
	        mkdir -p $targetPath
	        cp -R $workDirSansSlash/$dirName/* $targetPath/
EOF
done
exec 0<&12 12<&-

echo

# 6) Push the node$nodeId.cfg file to each cluster node's working directory.
echo "...copy the node$nodeId.cfg files to the machines' ($workDir/$workDirSansSlash/$dirName) for this cluster "
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
    echo "...copying $cfgFile for nodeId $id to $machine:$targetPath"
    scp "$cfgFile" "$machine:$targetPath/"
done
exec 0<&12 12<&-

echo


# 8) clean up
# echo "...clean up "
# exec 12<&0 # save current stdin
# exec < "$workDir/$ipPathPairFile"
# while read LINE; do
#     machine=$LINE
#     read LINE
#     targetPath=$LINE
# 	ssh -T $machine  <<-EOF
# 	        rm -Rf $workDir/$workDirSansSlash
#			rm -f "$workDir/$tarName"
# EOF
# done
# exec 0<&12 12<&-


