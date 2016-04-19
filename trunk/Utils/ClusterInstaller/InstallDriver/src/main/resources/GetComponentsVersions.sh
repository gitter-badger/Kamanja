#!/bin/bash

# GetComponentsVersions.sh

script_dir=$(dirname "$0")

name1=$1

Usage()
{
    echo
    echo "Usage:"
    echo "      GetComponentsVersions.sh --componentVersionJarAbsolutePath <componentVersionJarAbsolutePath> "
    echo "                               --componentVersionJarFileName <componentVersionJarFileName> "
    echo "                               --remoteNodeIp <remoteNodeIp> "
    echo "                               --resultsFileAbsolutePath <resultsFileAbsolutePath> "
    echo "                               --resultFileName <resultFileName> "
    echo "                               --jsonArg <jsonArg> "
    echo "                               --rootDirPath <rootDirPath> "
    echo "                               --pathOutputFileName <pathOutputFlName> "
    echo "                               --ignoreGetComponentsInfo <true if you want to ignore the call GetComponent-1.0 jar> "
    echo 
}

# Collect the named parameters 
componentVersionJarAbsolutePath=""
componentVersionJarFileName=""
remoteNodeIp=""
resultsFileAbsolutePath=""
resultFileName=""
jsonArg="" 
rootDirPath=""
pathOutputFileName=""
ignoreGetComponentsInfo=""

while [ "$1" != "" ]; do
    case $1 in
        --componentVersionJarAbsolutePath )   shift
                                componentVersionJarAbsolutePath=$1
                                ;;
        --componentVersionJarFileName )    shift
                                componentVersionJarFileName=$1
                                ;;
        --remoteNodeIp )      shift
                                remoteNodeIp=$1
                                ;;
        --resultsFileAbsolutePath )         shift
                                resultsFileAbsolutePath=$1
                                ;;
        --resultFileName )          shift
                                resultFileName=$1
                                ;;
        --rootDirPath )          shift
                                rootDirPath=$1
                                ;;
        --jsonArg )          shift
                                jsonArg=$1
                                ;;
        --ignoreGetComponentsInfo )          shift
                                ignoreGetComponentsInfo=$1
                                ;;
        --pathOutputFileName )          shift
                                pathOutputFileName=$1
                                ;;
        * )                     echo 
                                echo "Problem: Argument $1 is invalid named parameter."
                                Usage
                                exit 1
                                ;;
    esac
    shift
done

echo "componentVersionJarAbsolutePath:$componentVersionJarAbsolutePath, componentVersionJarFileName:$componentVersionJarFileName, remoteNodeIp:$remoteNodeIp, resultsFileAbsolutePath:$resultsFileAbsolutePath"
echo "resultFileName:$resultFileName, rootDirPath:$rootDirPath, pathOutputFileName:$pathOutputFileName, ignoreGetComponentsInfo:$ignoreGetComponentsInfo, jsonArg:$jsonArg"

rm -rf /tmp/Get-Component.log

if [ "$ignoreGetComponentsInfo" == "true" ]; then
	echo "Ignored GetComponentsInfo -- Ignored copying jar"
	echo "" > "$resultsFileAbsolutePath"
else
    scp -o StrictHostKeyChecking=no "$componentVersionJarAbsolutePath" "$remoteNodeIp:/tmp/$componentVersionJarFileName"
fi

ssh -o StrictHostKeyChecking=no -T $remoteNodeIp  <<-EOF
    if [ -n "~/.bash_profile" ]; then
        source ~/.bash_profile
    elif [ -n "~/.bashrc" ]; then
        source ~/.bashrc    
    fi
    
    echo "" > /tmp/Get-Component.log
    if [ "$ignoreGetComponentsInfo" == "true" ]; then
        echo "Ignored GetComponentsInfo -- Ignored execution"
    else
        rm -rf /tmp/${resultFileName}_local
        java -jar /tmp/$componentVersionJarFileName '/tmp/${resultFileName}_local' '$jsonArg'
    fi
	if [ -d "$rootDirPath" ]; then
		cd $rootDirPath
		echo "\$(pwd -P)" > "/tmp/${pathOutputFileName}_local"
	else
		echo "" > "/tmp/${pathOutputFileName}_local"
	fi
EOF

if [ "$ignoreGetComponentsInfo" == "true" ]; then
	echo "Ignored GetComponentsInfo -- Ignored getting file $remoteNodeIp:/tmp/${resultFileName}_local to $resultsFileAbsolutePath"
else
    scp -o StrictHostKeyChecking=no "$remoteNodeIp:/tmp/${resultFileName}_local" "$resultsFileAbsolutePath"
fi

    scp -o StrictHostKeyChecking=no "$remoteNodeIp:/tmp/${pathOutputFileName}_local" "/tmp/$pathOutputFileName"
scp -o StrictHostKeyChecking=no "$remoteNodeIp:/tmp/Get-Component.log" "/tmp/Get-Component.log"

