#!/bin/bash

# StatusKamanjaCluster.sh
#

Usage()
{
    echo 
    echo "Usage:"
    echo "      StatusKamanjaCluster.sh --ClusterId <cluster name identifer> "
    echo "                           --MetadataAPIConfig  <metadataAPICfgPath>  "
    echo 
    echo "  NOTES: Get status on the cluster specified by the cluster identifier parameter.  Use the metadata api configuration to "
    echo "         locate the appropriate metadata store.   "
    echo 
}


scalaversion="2.11"
name1=$1

if [ "$#" -eq 4 ]; then
	echo
else 
    echo "Problem: Incorrect number of arguments"
    echo 
    Usage
    exit 1
fi

if [[ "$name1" != "--MetadataAPIConfig" && "$name1" != "--ClusterId" ]]; then
	echo "Problem: Bad arguments"
	echo 
	Usage
	exit 1
fi

# Collect the named parameters 
metadataAPIConfig=""
clusterId=""

while [ "$1" != "" ]; do
    echo "parameter is $1"
    case $1 in
        --ClusterId )           shift
                                clusterId=$1
                                ;;
        --MetadataAPIConfig )   shift
                                metadataAPIConfig=$1
                                ;;
        * )                     echo "Problem: Argument $1 is invalid named parameter."
                                Usage
                                exit 1
                                ;;
    esac
    shift
done

# 1) Collect the relevant node information for this cluster.
workDir="/tmp" 
ipFile="ip.txt"
ipPathPairFile="ipPath.txt"
ipIdCfgTargPathQuartetFileName="ipIdCfgTarg.txt"
installDir=`cat $metadataAPIConfig | grep '[Rr][Oo][Oo][Tt]_[Dd][Ii][Rr]' | sed 's/.*=\(.*\)$/\1/g'`

echo "...extract node information for the cluster to be started from the Metadata configuration information supplied"

# info is assumed to be present in the supplied metadata store... see trunk/utils/NodeInfoExtract for details 
echo "...Command = java -cp $installDir/lib/system/jarfactoryofmodelinstancefactory_2.10-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs2_2.10-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs_2.10-1.4.0.jar:$installDir/lib/system/KamanjaInternalDeps_2.10-1.4.0.jar:$installDir/lib/system/nodeinfoextract_2.10-1.4.0.jar com.ligadata.installer.NodeInfoExtract --MetadataAPIConfig \"$metadataAPIConfig\" --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\" --installDir \"$installDir\" --clusterId \"$clusterId\""
java -cp $installDir/lib/system/jarfactoryofmodelinstancefactory_2.10-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs2_2.10-1.4.0.jar:$installDir/lib/system/ExtDependencyLibs_2.10-1.4.0.jar:$installDir/lib/system/KamanjaInternalDeps_2.10-1.4.0.jar:$installDir/lib/system/nodeinfoextract_2.10-1.4.0.jar com.ligadata.installer.NodeInfoExtract --MetadataAPIConfig $metadataAPIConfig --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName" --installDir "$installDir" --clusterId "$clusterId"
if [ "$?" -ne 0 ]; then
    echo
    echo "Problem: Invalid arguments supplied to the NodeInfoExtract-1.0 application... unable to obtain node configuration... exiting."
    Usage
    exit 1
fi

# Start the cluster nodes using the information extracted from the metadata and supplied config.  Remember the jvm's pid in the $installDir/run
# directory setup for that purpose.  The name of the pid file will always be 'node$id.pid'.  The targetPath points to the given cluster's 
# config directory where the Kamanja engine config file is located.
echo "...get stuatus for the Kamanja cluster $clusterName"
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
    read LINE
    roles=$LINE
    roles_lc=`echo $roles | tr '[:upper:]' '[:lower:]'`
    restapi_cnt=`echo $roles_lc | grep "restapi" | grep -v "grep" | wc -l`
    processingengine_cnt=`echo $roles_lc | grep "processingengine" | grep -v "grep" | wc -l`
    echo "NodeInfo = $machine, $id, $cfgFile, $targetPath, $roles"
    #scp -o StrictHostKeyChecking=no "$cfgFile" "$machine:$targetPath/"
    # 
    # FIXME: something more graceful than killing the jvm is desirable.
    #
    pidfile=node$id.pid
    statusfile=ndstatus$id.txt
    scp -o StrictHostKeyChecking=no "$machine:$installDir/run/$pidfile" "$workDir/$pidfile"
    pidvals=`head -1 "$workDir/$pidfile"`

    enginestatuspidcnt=0
    metadatastatuspidcnt=0

    # FIXME: We can check whether we really have pidvals or not and do ssh

    rm -rf "$workDir/$statusfile"

    # Checking whether the PID is valid & our service
    if [ ! -z "$pidvals" ]; then
       if [ -n "$pidvals" ]; then
          ssh -o StrictHostKeyChecking=no -T $machine  <<-EOF
             if [ ! -d "$installDir/run" ]; then
                mkdir "$installDir/run"
             fi
             ps u -p $pidvals | grep "KamanjaManager-1.0" | grep -v "grep" | wc -l > "$installDir/run/$statusfile"
             ps u -p $pidvals | grep "MetadataAPIService-1.0" | grep -v "grep" | wc -l >> "$installDir/run/$statusfile"

EOF

          scp -o StrictHostKeyChecking=no "$machine:$installDir/run/$statusfile" "$workDir/$statusfile"
          enginestatuspidcnt=`head -1 "$workDir/$statusfile"`
          metadatastatuspidcnt=`head -2 "$workDir/$statusfile" | tail -1`
        fi
    fi

    if [ "$processingengine_cnt" -gt 0 ]; then
    if [ ! -z "$enginestatuspidcnt" ]; then
        if [[ -n "$enginestatuspidcnt" ]] && [[ "$enginestatuspidcnt" -gt 0 ]]; then
            echo "Status:UP, Node:$machine, Service:KamanjaManager"
        else
            echo "Status:DOWN, Node:$machine, Service:KamanjaManager"
         fi
    else
         echo "Status:DOWN, Node:$machine, Service:KamanjaManager"
    fi
    fi

#    if [ "$restapi_cnt" -gt 0 ]; then
#    if [ ! -z "$metadatastatuspidcnt" ]; then
#        if [[ -n "$metadatastatuspidcnt" ]] && [[ "$metadatastatuspidcnt" -gt 0 ]]; then
#            echo "Status:UP, Node:$machine, Service:MetadataAPIService"
#        else
#            echo "Status:DOWN, Node:$machine, Service:MetadataAPIService"
#         fi
#    else
#         echo "Status:DOWN, Node:$machine, Service:MetadataAPIService"
#    fi
#    fi

done
exec 0<&12 12<&-

echo

