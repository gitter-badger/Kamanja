#!/bin/bash

installPath=$1
srcPath=$2
configFile=$3

if [ ! -d "$installPath" ]; then
        echo "No install path supplied. Usage: PrepKvStores.sh <installPath> <sourcePath> <engineConfig>"
        exit 1
fi

if [ ! -d "$srcPath" ]; then
        echo "No source path supplied. Usage: PrepKvStores.sh <installPath> <sourcePath> <engineConfig>"
        exit 1
fi

if [ ! -d "$configFile"]; then
	echo "No configuration file supplied. Usage: PrepKvStores.sh <installPath> <sourcePath> <engineConfig>"
	exit 1
fi
export KAMANJALIBPATH=$installPath

for file in $installPath/*.jar;
do
	echo "Adding $file to classpath..."
	CLASSPATH=$CLASSPATH:$file
done

echo "Prepare the test kvstore - Dyspnea Codes map..."

java -jar $KAMANJALIBPATH/KVInit-1.0 --kvname System.DyspnoeaCodes --config $configFile --csvpath $srcPath/SampleApplication/Medical/SampleData/dyspnoea.csv --keyfieldname icd9Code

echo "Prepare the test kvstore - Environmental Exposure Codes map..."

java -jar $KAMANJALIBPATH/KVInit-1.0 --kvname System.EnvCodes --config $configFile --csvpath $srcPath/SampleApplication/Medical/SampleData/envExposureCodes.csv --keyfieldname icd9Code

echo "Prepare the test kvstore - Sputum Codes map..."

java -jar $KAMANJALIBPATH/KVInit-1.0 --kvname System.SputumCodes --config $configFile --csvpath $srcPath/SampleApplication/Medical/SampleData/sputumCodes.csv --keyfieldname icd9Code

echo "Prepare the test kvstore - Cough Codes map..."

java -jar $KAMANJALIBPATH/KVInit-1.0 --kvname System.CoughCodes --config $configFile --csvpath $srcPath/SampleApplication/Medical/SampleData/coughCodes.csv --keyfieldname icd9Code

echo "Prepare the test kvstore - Smoking Codes map..."

java -jar $KAMANJALIBPATH/KVInit-1.0 --kvname System.SmokeCodes --config $configFile --csvpath $srcPath/SampleApplication/Medical/SampleData/smokingCodes.csv --keyfieldname icd9Code