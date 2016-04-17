#!/bin/bash

# KamanjaClusterInstall.sh

#   Based upon either the metadata at the location found specified in the MetadataAPI config supplied, or, if a 
#   node config file was specified, from that file... install the Kamanja software.  The software is 
#   built with the easy installer script and then gzipped and tar'd when the KafkaInstallPath is supplied.
#   If the TarballPath option is given, the tarball path in the value will be distributed.  If both Kafka path and
#   TarballPath are given, script issues usage message and exits.
#
#   Build examples:
#       a) Using the node config file Engine2BoxConfigV1.json 
#       KamanjaClusterInstall.sh  --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties 
#                               --NodeConfigPath SampleApplication/Medical/Configs/Engine2BoxConfigV1.json 
#                               --KafkaInstallPath ~/tarballs/kafka/2.11/kafka_2.11-0.8.1.1
#       b) Using the metadata found in the metadata store specified by the MetadataAPIConfig.properties
#       KamanjaClusterInstall.sh  --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties 
#                               --KafkaInstallPath ~/tarballs/kafka/2.11/kafka_2.11-0.8.1.1
#
#   TarballPath distribution examples (when the tarball has been built outside this script):
#       a) Using the node config file Engine2BoxConfigV1.json 
#       KamanjaClusterInstall.sh  --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties 
#                               --NodeConfigPath SampleApplication/Medical/Configs/Engine2BoxConfigV1.json 
#                               --TarballPath ~/tarballs/Kamanja-01.00.0001.tgz
#       b) Using the metadata found in the metadata store specified by the MetadataAPIConfig.properties
#       KamanjaClusterInstall.sh  --MetadataAPIConfig SampleApplication/Medical/Configs/MetadataAPIConfig.properties 
#                               --TarballPath ~/tarballs/Kamanja-01.00.0001.tgz
#
#   In the "a)" examples, a cluster configuration is presumably presented in the NodeConfigPath file (i.e, the cluster decl
#   is new).  In the "b)" examples, the cluster config info is retrieved from the metadata store.
#
#   If you supply all of the options, you will be asked to try again.  An alternate working directory may be supplied.  This 
#   directory is used by this script to store the built software when compiling as well as the control files that are generated
#   by the NodeInfoExtract.  By default, "/tmp" is used.  If a special one is supplied as the WorkingDir parameter value, it
#   must exist.  The user account must have CRUD access to its content.
#
#   If a cluster config is not present there when no NodeConfigPath argument is presented, an exception is thrown.  In fact
#   if the cluster map returned by the metadata api is empty, one is thrown in any case.  The other reason is that the 
#   engine cluster config is messed up in the NodeConfigPath file supplied.
#
#   NOTE: Only tar'd gzip files supported at the moment for the tarballs.
#

script_dir=$(dirname "$0")

scalaversion="2.11"
name1=$1

Usage()
{
    echo
    echo "Install a Kamanja cluster from source or tarball."
    echo
    echo "Usage if building from source:"
    echo "      KamanjaClusterInstall.sh --ClusterId <cluster name identifer> "
    echo "                               --MetadataAPIConfig  <metadataAPICfgPath>  "
    echo "                               --KafkaInstallPath <kafka location>"
    echo "                               --NodeConfigPath <engine config path> "
    echo "                               [ --WorkingDir <alt working dir>  ]"
    echo "Usage if deploying tarball:"
    echo "      KamanjaClusterInstall.sh --ClusterId <cluster name identifer> "
    echo "                               --MetadataAPIConfig  <metadataAPICfgPath>  "
    echo "                               --TarballPath <tarball path>"
    echo "                               --NodeConfigPath <engine config path> "
    echo "                               [ --WorkingDir <alt working dir>  ]"
    echo "                               --ipAddrs <the ips in the cluster>  "
    echo "                               --ipIdTargPaths <unique ips, node ids, target paths, and roles>  "
    echo "                               --ipPathPairs <unique ips and target paths>  "
    echo "                               --priorInstallDirPath <name to use for rename of prior install if any>  "
    echo "                               --newInstallDirPath <new dir path of physical install>  "
    echo "                               --installVerificationFile <file to get the verification information>  "
    echo "                               --externalJarsDir <external jars directory to be copied to installation lib/application> "
    echo "                               --tenantId <a tenantId is applied to all metadata objects> "
    echo
    echo "  NOTES: Only tar'd gzip files are supported for the tarballs at the moment."
    echo "         NodeConfigPath must be supplied always"
    echo "         The working directory, by default, is /tmp.  If such a public location is abhorrent, chose a private one.  It"
    echo "         must be an existing directory and readable by this script, however"
    echo "         If both the KafkaInstallPath and the TarballPath are specified, the script fails."
    echo "         If neither the KafkaInstallPath or TarballPath  is supplied, the script will fail. "
    echo "         A ClusterId is a required argument.  It will use only the nodes identified with that cluster id."
    echo
    echo "         In addition, the NodeInfoExtract application that is used by this installer to fetch cluster node configuration  "
    echo "         information must be on the PATH.  It is found in the trunk/Utils/NodeInfoExtract/target/scala-$scalaversion/ "
    echo
    echo 
}


# Check 1: Is this even close to reasonable?
if [[ "$#" -eq 1  || "$#" -eq 4  || "$#" -eq 6  || "$#" -eq 8  || "$#" -eq 10  || "$#" -eq 12  || "$#" -eq 14  || "$#" -eq 16  || "$#" -eq 18  || "$#" -eq 20 || "$#" -eq 22 || "$#" -eq 24 || "$#" -eq 26 || "$#" -eq 28 ]]; then
    echo 
else 
    echo 
    echo "Problem: Incorrect number of arguments"
    Usage
    exit 1
fi

# Check 2: Is this even close to reasonable?
if [[ "$name1" != "--ClusterId" && "$name1" != "--MetadataAPIConfig" && "$name1" != "--NodeConfigPath"  && "$name1" != "--KafkaInstallPath"   && "$name1" != "--TarballPath"  && "$name1" != "--WorkingDir"   && "$name1" != "--ipAddrs"   && "$name1" != "--ipIdTargPaths"   && "$name1" != "--ipPathPairs" && "$name1" != "--priorInstallDirPath" &&  "$name1" != "--newInstallDirPath"  &&  "$name1" != "--externalJarsDir" &&  "$name1" != "--tenantId" ]]; then
    echo 
	echo "Problem: Unreasonable number of arguments... as few as 2 and as many as 26 may be supplied."
    Usage
	exit 1
fi

# Collect the named parameters 
metadataAPIConfig=""
kafkaInstallPath=""
nodeConfigPath=""
tarballPath=""
nodeCfgGiven=""
workDir="/tmp"
installDirName="" 
clusterId=""
ipAddrs=""
ipIdTargPaths=""
ipPathPairs=""
priorInstallDirPath=""
newInstallDirPath=""
installVerificationFile=""
externalJarsDir=""
tenantId=""

while [ "$1" != "" ]; do
    case $1 in
        --MetadataAPIConfig )   shift
                                metadataAPIConfig=$1
                                ;;
        --KafkaInstallPath )    shift
                                kafkaInstallPath=$1
                                ;;
        --NodeConfigPath )      shift
                                nodeConfigPath=$1
                                nodeCfgGiven="true enough"
                                ;;
        --TarballPath )         shift
                                tarballPath=$1
                                ;;
        --WorkingDir )          shift
                                workDir=$1
                                ;;
        --ClusterId )           shift
                                clusterId=$1
                                ;;
        --ipAddrs )           	shift
                                ipAddrs=$1
                                ;;
        --ipIdTargPaths )       shift
                                ipIdTargPaths=$1
                                ;;
        --ipPathPairs )         shift
                                ipPathPairs=$1
                                ;;
        --priorInstallDirPath ) shift
                                priorInstallDirPath=$1
                                ;;
        --newInstallDirPath )   shift
                                newInstallDirPath=$1
                                ;;
        --installVerificationFile )   shift
                                installVerificationFile=$1
                                ;;
        --externalJarsDir )   shift
                                externalJarsDir=$1
                                ;;
        --tenantId )           shift
                                tenantId=$1
                                ;;
        --help )           		Usage
        						exit 0
                                ;;
        * )                     echo
                                echo "Problem: Argument $1 is invalid named parameter."
                                Usage
                                exit 1
                                ;;
    esac
    shift
done

# Check 3: Is this even close to reasonable?
currDirPath=`pwd`
currDir=`echo "$currDirPath" | sed 's/.*\/\(.*\)/\1/g'`
#if [ "$currDir" != "trunk" -a "$tarballPath" != "" ]; then
#	echo 
#	echo "Problem: Currently if building installation from source, this script must be run from the trunk directory of the "
#	echo "valid local git repo containing the desired software version."
#	echo
#	echo "This is the current directory : $currDir"
#	echo
#	Usage
#	exit 1
#fi

# Check 4: Is this even close to reasonable?
echo "tarballPath = $tarballPath"
echo "kafkaInstallPath = $kafkaInstallPath"

echo "$ipAddrs"
echo "$ipPathPairs"
echo "$ipIdTargPaths"
if [ -n "$tarballPath" -a -n "$kafkaInstallPath" ]; then
    echo 
    echo "Problem: Either install from source or use the tarball specification... just don't do both on same run."
    Usage
    exit 1
fi

# Check 5: if the working directory was given, make sure it is full qualified
if [ -z "$tarballPath" -a -z "$kafkaInstallPath" ]; then
    echo 
    echo "Problem: Installation impossible. Specify --KafkaInstallPath to install from sources."
    echo "         Alternatively, specify a --TarballPath to install your tarball."
    Usage
    exit 1
fi

# Check 6: if the working directory was given, make sure it is full qualified
workDirHasLeadSlash=`echo "$workDir" | grep '^\/.*'`
if [ -z "$workDirHasLeadSlash" ]; then
    echo 
    echo "Problem: The WorkingDir must be a fully qualified path."
    Usage
    exit 1
fi

# if installVerificationFile option is not given, generating file InstallVerificationFile.txt in $workDir
if [ "$installVerificationFile" == "" ]; then
	installVerificationFile="$workDir/InstallVerificationFile.txt"
fi

rm -f $installVerificationFile

# Creating working directory
mkdir -p $workDir

echo "" > $workDir/InstallStatus.txt

# Check 7: working directory must exist
if [ ! -d "$workDir" ]; then
    echo 
    echo "Problem: The WorkingDir must exist and be a directory."
    Usage
    exit 1
fi

if [ -n "$tarballPath" ]; then
    # Check 8: tarball path must exist
    if [ ! -f "$tarballPath" ]; then
        echo 
        echo "Problem: The TarballPath must exist and be a regular file."
        Usage
        exit 1
    fi

    # Check 9: tarball path must be readable
    if [ ! -r "$tarballPath" ]; then
        echo 
        echo "Problem: The TarballPath must be readable."
        Usage
        exit 1
    fi
fi

if [ -n "$kafkaInstallPath" ]; then
    # Check 10: Is Kafka legit?
    if [ ! -d "$kafkaInstallPath" ]; then
        echo 
        echo "Problem: KafkaInstallPath must exist."
        Usage
        exit 1
    fi
    # Check 11: Is Kafka legit?
    if [ ! -f "$kafkaInstallPath/bin/kafka-server-start.sh" ]; then
        echo 
        echo "Problem: KafkaInstallPath $kafkaInstallPath doesn't look right... where is bin/kafka-server-start.sh?"
        Usage
        exit 1
    fi
fi

# Check 12: Does the metadata api config exist?
if [ ! -f "$metadataAPIConfig" ]; then
    echo 
    echo "Problem: The MetadataAPIConfig $metadataAPIConfig doesn't exist... please refer to a valid metadata api configuration file"
    Usage
    exit 1
fi

# Check 13: If the node config was given, does it exist?
if [ -n "$nodeConfigPath" ]; then
    if [ ! -f "$nodeConfigPath" ]; then
        echo 
        echo "Problem: The supplied NodeConfigPath $nodeConfigPath doesn't exist... please refer to a valid node configuration file"
        Usage
        exit 1
    fi
else
        echo 
        echo "Problem: NodeConfigPath was not supplied. Please refer to a valid node configuration file"
        Usage
        exit 1
fi

# Check 14: There must be a clusterId, and if a nodeConfigPath is specified, it must be the same value as the ClusterId value found there
if [ -n "$clusterId" ]; then
    if [ -f "$nodeConfigPath" ]; then
        numberClusters=`cat $nodeConfigPath | grep '[Cc][lL][uU][sS][tT][eE][rR][Ii][dD]' | sed 's/.*:[ \t][ \t]*\"\(.*\)\".*/\1/g' | wc -l`
        if [ "$numberClusters" -ne 1 ]; then
            echo 
            echo "Problem: The $nodeConfigPath has more that one cluster definition in it.  That is not supported.  Create a node config with the desired cluster declaration and resubmit."
            Usage
            exit 1
        fi
        
        nodeCfgClusterName=`cat $nodeConfigPath | grep '[Cc][lL][uU][sS][tT][eE][rR][Ii][dD]' | sed 's/.*:[ \t][ \t]*\"\(.*\)\".*/\1/g'`
        # case insensitive compare (bash 4x assumed...)
        if [ "${nodeCfgClusterName,,}" != "${clusterId,,}" ]; then
            echo 
            echo "Problem: The supplied cluster identifier ($clusterId) must be same as one in $nodeConfigPath (i.e., $nodeCfgClusterName) when the node config is being supplied with a node configuration file."
            Usage
            exit 1
        fi
    fi
else
    echo 
    echo "Problem: The ClusterId must be supplied to select the nodes for use in the installation.  This is needed since multiple clusters can"
    echo "         be defined in the same metadata store."
    Usage
    exit 1
fi


# Check N: more checks could probably be added ... 


# Skip the build if tarballPath was supplied 
dtPrefix="Kamanja`date +"%Y%b%d"`"
tarName="$dtPrefix.tgz"
trunkDir=`pwd` #save the current trunk directory 

installDir=`cat $metadataAPIConfig | grep '[Rr][Oo][Oo][Tt]_[Dd][Ii][Rr]' | sed 's/.*=\(.*\)$/\1/g'`
installDir=`echo "$installDir" | sed -e 's/\/[\/]*$//'`
installDirName=`echo $installDir | sed 's/.*\/\(.*\)$/\1/g'`
installFolder=`echo "$installDir" | sed -e 's/\/[^\/]*$//'`

if [ -d "$installFolder" ]; then 
	echo "$installFolder is a Folder"
else
	echo "$installFolder does not exist"
	Usage
	exit 1
fi

if [ -z "$tarballPath" ]; then
	# 1 build the installation in the staging directory
	stagingDir="$workDir/$installDirName"
	mkdir -p "$stagingDir"
	echo "...build the Kamanja installation directory in $stagingDir"

    # use the install directory given in the metadataAPI config file's ROOT_DIR's value
    # we will use assume the current user's .ivy2 directory for the deps and the `pwd` for the build directory.
    # the KafkaInstallPath supplied will be used for kafka
    echo "...building the repo found in `pwd` staging to $stagingDir.  Each cluster node will have this build installed in $installDir."
    easyInstallKamanja.sh "$stagingDir" `pwd` ~/.ivy2 "$kafkaInstallPath"

    # 2) compress staging dir and tar it
    echo "...compress and tar the installation directory $stagingDir to $tarName"
    cd "$workDir"
    tar czvf "$workDir/$tarName" "$installDirName"
    cd "$trunkDir"

    tarballPath="$workDir/$tarName"
else
    # get the tarball file name 
    tarName=`echo "$tarballPath" | sed 's/.*\/\(.*\)/\1/g'`
fi

# 3) determine which machines and installation directories are to get the build from the metadata and Kamanja config

oldWay="false"

if [ "$ipAddrs" == "" -a "$ipIdTargPaths" == ""  -a "$ipPathPairs" == ""  -a "$priorInstallDirPath" == ""  -a "$newInstallDirPath" == "" ]; then
	# follow the old way
	ipTmpFile="ip.txt"
	ipPathPairTmpFile="ipPath.txt"
	ipIdCfgTargPathQuartetTmpFileName="ipIdCfgTarg.txt"

	echo "...extract node information for the cluster to be installed from the Metadata configuration and optional node information supplied"
	if  [ -n "$nodeCfgGiven" ]; then
		echo "metadataAPIConfig = $metadataAPIConfig"
		echo "nodeConfigPath = $nodeConfigPath"
		echo "workDir = $workDir"
		echo "ipTmpFile = $ipTmpFile"
		echo "ipPathPairTmpFile = $ipPathPairTmpFile"
		echo "ipIdCfgTargPathQuartetTmpFileName = $ipIdCfgTargPathQuartetTmpFileName"
		echo "installDir = $installDir"
		echo "clusterId = $clusterId"
		echo "...Command = java -cp $installDir/lib/system/jarfactoryofmodelinstancefactory_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:$installDir/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:$installDir/lib/system/nodeinfoextract_2.11-1.4.0.jar com.ligadata.installer.NodeInfoExtract --MetadataAPIConfig \"$metadataAPIConfig\" --NodeConfigPath \"$nodeConfigPath\"  --workDir \"$workDir\" --ipFileName \"$ipTmpFile\" --ipPathPairFileName \"$ipPathPairTmpFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetTmpFileName\" --installDir \"$installDir\" --clusterId \"$clusterId\""
		java -cp $installDir/lib/system/jarfactoryofmodelinstancefactory_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:$installDir/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:$installDir/lib/system/nodeinfoextract_2.11-1.4.0.jar com.ligadata.installer.NodeInfoExtract --MetadataAPIConfig "$metadataAPIConfig" --NodeConfigPath "$nodeConfigPath" --workDir "$workDir" --ipFileName "$ipTmpFile" --ipPathPairFileName "$ipPathPairTmpFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetTmpFileName"  --installDir "$installDir" --clusterId "$clusterId"
		# Check 15: Bad NodeInfoExtract-1.0 arguments
		if [ "$?" -ne 0 ]; then
			echo
			echo "Problem: Invalid arguments supplied to the NodeInfoExtract-1.0 application... unable to obtain node configuration... exiting."
			Usage
			exit 1
		fi
	else # info is assumed to be present in the supplied metadata store... see trunk/utils/NodeInfoExtract for details
		echo "...Command = $nodeInfoExtractDir/NodeInfoExtract-1.0 --MetadataAPIConfig \"$metadataAPIConfig\" --workDir \"$workDir\" --ipFileName \"$ipTmpFile\" --ipPathPairFileName \"$ipPathPairTmpFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetTmpFileName\" --installDir \"$installDir\" --clusterId \"$clusterId\""
			java -cp $installDir/lib/system/jarfactoryofmodelinstancefactory_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs2_2.11-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs_2.11-1.4.0.jar:$installDir/lib/system/KamanjaInternalDeps_2.11-1.4.0.jar:$installDir/lib/system/nodeinfoextract_2.11-1.4.0.jar com.ligadata.installer.NodeInfoExtract --MetadataAPIConfig $metadataAPIConfig --workDir "$workDir" --ipFileName "$ipTmpFile" --ipPathPairFileName "$ipPathPairTmpFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetTmpFileName" --installDir "$installDir" --clusterId "$clusterId"
		# Check 15: Bad NodeInfoExtract-1.0 arguments
		if [ "$?" -ne 0 ]; then
			echo
			echo "Problem: Invalid arguments supplied to the NodeInfoExtract-1.0 application... unable to obtain node configuration... exiting."
			Usage
			exit 1
		fi
	fi

	ipFile="$workDir/$ipTmpFile"
	ipPathPairFile="$workDir/$ipPathPairTmpFile"
	ipIdCfgTargPathQuartetFileName="$workDir/$ipIdCfgTargPathQuartetTmpFileName"
	oldWay="true"
else
	# A number of files are produced, all USED TO BE IN WORKING DIR WHEN THEY WERE created by the NodeInfoExtract.  Now
	# these three files are passed in as full paths from the caller.  NodeInfoExtract is not used.
	ipFile="$ipAddrs"
	ipPathPairFile="$ipPathPairs"
	ipIdCfgTargPathQuartetFileName="$ipIdTargPaths"
fi
echo "$ipFile"
echo "$ipPathPairFile"
echo "$ipIdCfgTargPathQuartetFileName"


echo "...creating directories to copy the tarball to the machines in this cluster"
exec 12<&0 # save current stdin
#exec < "$workDir/$ipFile"
exec < "$ipFile"
while read LINE; do
    machine=$LINE
    echo "...creating directory $machine:$workDir"
    ssh -o StrictHostKeyChecking=no -T $machine <<-EOF
        mkdir -p $workDir
EOF
 done
exec 0<&12 12<&-


# 4) Push the tarballs to each machine defined in the supplied configuration
echo "...copy the tarball to the machines in this cluster"
exec 12<&0 # save current stdin
#exec < "$workDir/$ipFile"
exec < "$ipFile"
while read LINE; do
    machine=$LINE
    echo "...copying $tarName to $machine:$workDir/$tarName"
    scp -o StrictHostKeyChecking=no "$tarballPath" "$machine:$workDir/$tarName"
done
exec 0<&12 12<&-

echo "...copy is done"

echo

DATE=`date +%Y%m%d%H%M%S`

# 5) untar/decompress tarballs there and move them into place
echo "...for each directory specified on each machine participating in the cluster, untar and decompress the software to $workDir/$installDirName... then move to corresponding target path"
priorInstallationDetected="false"
brokenLink="false"

exec 12<&0 # save current stdin
#exec < "$workDir/$ipPathPairFile"
exec < "$ipPathPairFile"
while read LINE; do
    machine=$LINE
    read LINE
    targetPath=`echo "$LINE" | sed -e 's/\/[\/]*$//'`
    targetFolder=`echo "$targetPath" | sed -e 's/\/[^\/]*$//'`
    targetPath_date="$targetPath"_"$DATE"
    if [ "$oldWay" == "true" ]; then
        priorInstallDirPath="$targetPath"_pre_"$DATE"
        newInstallDirPath="$targetPath_date"
    fi
	
    echo "On node $machine extract the tarball $tarName and copy it to $targetPath iff $workDir/$installDirName != $targetPath"
	echo "Values before execute on node $machine => TargetPath: $targetPath, PriorInstallDirPath: $priorInstallDirPath, NewInstallDirPath:  $newInstallDirPath"

	ssh -o StrictHostKeyChecking=no -T $machine  <<-EOF
		if [ -d "$targetFolder" ]; then 
			cd $workDir
			if [ ! -L $targetPath ]; then
				if [ -d "$targetPath" ]; then
					mv 	"$targetPath" "$priorInstallDirPath"
					echo "true,false" > "$workDir/curNodeLinksInfoLocal.txt"
				else
					echo "false,false" > "$workDir/curNodeLinksInfoLocal.txt"
				fi
			else
				cd $targetPath
				echo "true,true,\$(pwd -P)" > "$workDir/curNodeLinksInfoLocal.txt"
				cd $workDir
				unlink $targetPath
			fi
			mkdir -p $newInstallDirPath
	 		tar xzf $tarName -C $newInstallDirPath --strip-components 1
			ln -sf  $newInstallDirPath $targetPath
	 	else
			echo "false,false" > "$workDir/curNodeLinksInfoLocal.txt"
			echo "$targetFolder is not directory"	
		fi
EOF

    scp -o StrictHostKeyChecking=no "$machine:$workDir/curNodeLinksInfoLocal.txt" "$workDir/curNodeLinksInfo.txt"
    
    curNodeFlags=$( cat "$workDir/curNodeLinksInfo.txt" )
    curNodeFlgsArr=(`echo $curNodeFlags | cut -d ","  --output-delimiter=" " -f 1-`)

	curNodePriorInstDetected=${curNodeFlgsArr[0]}
	curNodeBrokenLnk=${curNodeFlgsArr[1]}
	foundPriorInstDir=${curNodeFlgsArr[2]}

	if [ "$curNodePriorInstDetected" == "true" ]; then
		priorInstallationDetected="true"
	fi

	if [ "$curNodeBrokenLnk" == "true" ]; then
		brokenLink="true"
	fi

	if [ "$externalJarsDir" != "" ]; then
        scp -o StrictHostKeyChecking=no "$externalJarsDir"/* "$machine:$newInstallDirPath/lib/application/"
	fi

	if [ "$curNodePriorInstDetected" == "false" ]; then
		echo "On node $machine prior installation not found at $targetPath. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
	else
		if [ "$curNodeBrokenLnk" == "false" ]; then
			echo "On node $machine found prior installation path at $targetPath as directory and now moved it to $priorInstallDirPath. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
		else
			echo "On node $machine found prior installation path at $targetPath as link to $foundPriorInstDir. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
		fi
	fi
done
exec 0<&12 12<&-

if [ "$priorInstallationDetected" == "false" ]; then
	echo "Prior installation not found at $targetPath. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
else
	if [ "$brokenLink" == "false" ]; then
		echo "Found prior installation path at $targetPath as directory and now moved it to $priorInstallDirPath. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
	else
		echo "Found prior installation path at $targetPath as link to $priorInstallDirPath. New installation is done at $newInstallDirPath and created link from $targetPath to $newInstallDirPath"
	fi
fi

echo "New Kamanja physical installation is $newInstallDirPath. The $targetPath now points to this installation."

echo

installDir_repl=$(echo $installDir | sed 's/\//\\\//g')

# 6) Push the node$nodeId.cfg file to each cluster node's working directory.
echo "...copy the node$nodeId.cfg & log files to the machines' ($workDir/$installDirName) for this cluster "
exec 12<&0 # save current stdin
#exec < "$workDir/$ipIdCfgTargPathQuartetFileName"
exec < "$ipIdCfgTargPathQuartetFileName"
while read LINE; do
    machine=$LINE
    read LINE
    id=$LINE
    read LINE
    cfgFile=$LINE
    read LINE
    targetPath=$LINE
    read LINE
    roles=$LINE
    echo "NodeInfo = $machine, $id, $cfgFile, $targetPath, $roles"
    echo "...copying $cfgFile for nodeId $id to $machine:$targetPath"
    scp -o StrictHostKeyChecking=no "$cfgFile" "$machine:$targetPath/"

    # Engine Logfile. For now all nodes log files are same. May be later we can change.
    sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/engine_log4j2_template.xml > $workDir/engine_log4j2.xml
    sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/restapi_log4j2_template.xml > $workDir/restapi_log4j2.xml
    sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/log4j2_template.xml > $workDir/log4j2.xml
    #sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/../config/engine_log4j2_template.xml > $workDir/engine_log4j2.xml
    #sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/../config/restapi_log4j2_template.xml > $workDir/restapi_log4j2.xml
    #sed "s/{InstallPath}/$installDir_repl/g;s/{NodeId}/$id/g" $script_dir/../config/log4j2_template.xml > $workDir/log4j2.xml
    sed "s/{NodeId}/$id/g;s/{HostName}/$machine/g" $metadataAPIConfig > $workDir/MetadataAPIConfig_${id}.properties
    scp -o StrictHostKeyChecking=no "$workDir/engine_log4j2.xml" "$machine:$targetPath/"
    scp -o StrictHostKeyChecking=no "$workDir/restapi_log4j2.xml" "$machine:$targetPath/"
    scp -o StrictHostKeyChecking=no "$workDir/log4j2.xml" "$machine:$targetPath/"
    scp -o StrictHostKeyChecking=no "$workDir/MetadataAPIConfig_${id}.properties" "$machine:$targetPath/MetadataAPIConfig_${id}.properties"
    # scp -o StrictHostKeyChecking=no "$workDir/MetadataAPIConfig_${id}.properties" "$machine:$targetPath/MetadataAPIConfig.properties"
done
exec 0<&12 12<&-

echo

# 7) Run SetPaths.sh & Copy given Node Config File (Ex: ClusterConfig.json)
echo "...for each machine set new paths"
exec 12<&0 # save current stdin
#exec < "$workDir/$ipPathPairFile"
exec < "$ipPathPairFile"
while read LINE; do
	machine=$LINE
	read LINE
	targetPath=$LINE
	echo "Running $machine:$targetPath/SetPaths.sh"
	ssh -o StrictHostKeyChecking=no -T $machine  <<-EOF
		if [ -n "~/.bash_profile" ]; then
			source ~/.bash_profile
		elif [ -n "~/.bashrc" ]; then
			source ~/.bashrc    
		fi
		bash $targetPath/bin/SetPaths.sh
EOF
    scp -o StrictHostKeyChecking=no "$nodeConfigPath" "$machine:$targetPath/config/ClusterConfig.json"
done
exec 0<&12 12<&-

echo

# 7.1) Copy $workDir/MetadataAPIConfig_${id}.properties to $machine:$targetPath/MetadataAPIConfig.properties
exec 12<&0 # save current stdin
#exec < "$workDir/$ipIdCfgTargPathQuartetFileName"
exec < "$ipIdCfgTargPathQuartetFileName"
while read LINE; do
    machine=$LINE
    read LINE
    id=$LINE
    read LINE
    cfgFile=$LINE
    read LINE
    targetPath=$LINE
    read LINE
    roles=$LINE
    
	if [ -n "$workDir/MetadataAPIConfig_${id}.properties" ]; then
		echo "...copying $workDir/MetadataAPIConfig_${id}.properties to $machine:$targetPath/MetadataAPIConfig.properties"
		scp -o StrictHostKeyChecking=no "$workDir/MetadataAPIConfig_${id}.properties" "$machine:$targetPath/MetadataAPIConfig.properties"
	fi
done
exec 0<&12 12<&-

echo

# 8) clean up
# echo "...clean up "
# exec 12<&0 # save current stdin
### exec < "$workDir/$ipPathPairFile"
# exec < "$ipPathPairFile"
# while read LINE; do
#     machine=$LINE
#     read LINE
#     targetPath=$LINE
# 	ssh -o StrictHostKeyChecking=no -T $machine  <<-EOF
#			rm -f "$workDir/$tarName"
# EOF
# done
# exec 0<&12 12<&-


# 9) Check on each node and Write the information for verification purpose
exec 12<&0 # save current stdin
exec < "$ipPathPairFile"
while read LINE; do
	machine=$LINE
	read LINE
	targetPath=$LINE

	ssh -o StrictHostKeyChecking=no -T $machine  <<-EOF
		# {HostName,LinkDir,LinkExists(Yes|No),LinkPointingToDir,LinkPointingDirExists(Yes|No),NewInstallDir, NewInstallDirExists(Yes|No)} 
		echo "" > $workDir/InstallStatusLocal.txt
		if [ -d "$targetPath" ]; then 
			if [ ! -L $targetPath ]; then
				cd $targetPath
				if [ -d "$newInstallDirPath" ]; then 
					echo "$machine,$targetPath,No,\$(pwd -P),Yes,$newInstallDirPath,Yes"	> "$workDir/InstallStatusLocal.txt"
				else
					echo "$machine,$targetPath,No,\$(pwd -P),Yes,$newInstallDirPath,No"	> "$workDir/InstallStatusLocal.txt"
				fi
			else
				cd $targetPath
				if [ -d "$newInstallDirPath" ]; then 
					echo "$machine,$targetPath,Yes,\$(pwd -P),Yes,$newInstallDirPath,Yes"	> "$workDir/InstallStatusLocal.txt"
				else
					echo "$machine,$targetPath,Yes,\$(pwd -P),Yes,$newInstallDirPath,No"	> "$workDir/InstallStatusLocal.txt"
				fi
			fi
		else
			if [ -d "$newInstallDirPath" ]; then 
				echo "$machine,$targetPath,No,,No,$newInstallDirPath,Yes"	> "$workDir/InstallStatusLocal.txt"
			else
				echo "$machine,$targetPath,No,,No,$newInstallDirPath,No"	> "$workDir/InstallStatusLocal.txt"
			fi
		fi
EOF
    scp -o StrictHostKeyChecking=no "$machine:$workDir/InstallStatusLocal.txt" "$workDir/InstallStatus.txt"
    cat "$workDir/InstallStatus.txt" >> $installVerificationFile
done
exec 0<&12 12<&-

echo

