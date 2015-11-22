#!/bin/bash

installPath=$1
srcPath=$2

echo $installPath
echo $srcPath

if [ ! -d "$installPath" ]; then
        echo "No install path supplied.  It should be a directory that can be written to and whose current content is of no value (will be overwritten) "
        echo "$0 <install path> <src tree trunk directory>"
        exit 1
fi

if [ ! -d "$srcPath" ]; then
        echo "No src path supplied.  It should be the trunk directory containing the jars, files, what not that need to be supplied."
        echo "$0 <install path> <src tree trunk directory>"
        exit 1
fi


export KAMANJALIBPATH=$installPath

# *******************************
# Clean out prior installation
# *******************************
rm -Rf $KAMANJALIBPATH

# *******************************
# Make the directories as needed
# *******************************
mkdir -p $KAMANJALIBPATH/msgdata
mkdir -p $KAMANJALIBPATH/kvstores
mkdir -p $KAMANJALIBPATH/logs

# *******************************
# Build fat-jars
# *******************************

echo "clean, package and assemble $srcPath ..."

cd $srcPath
sbt clean package KamanjaManager/assembly MetadataAPI/assembly KVInit/assembly MethodExtractor/assembly SimpleKafkaProducer/assembly NodeInfoExtract/assembly
#sbt package 
#sbt KamanjaManager/assembly 
#sbt MetadataAPI/assembly 
#sbt KVInit/assembly 

# recreate eclipse projects
#echo "refresh the eclipse projects ..."
#cd $srcPath
#sbt eclipse

# Move them into place
echo "copy the fat jars to $KAMANJALIBPATH ..."

cd $srcPath
cp Utils/KVInit/target/scala-2.11/KVInit* $KAMANJALIBPATH
cp MetadataAPI/target/scala-2.11/MetadataAPI* $KAMANJALIBPATH
cp KamanjaManager/target/scala-2.11/KamanjaManager* $KAMANJALIBPATH
cp Pmml/MethodExtractor/target/scala-2.11/MethodExtractor* $KAMANJALIBPATH
cp Utils/SimpleKafkaProducer/target/scala-2.11/SimpleKafkaProducer* $KAMANJALIBPATH

# *******************************
# Copy jars required (more than required if the fat jars are used)
# *******************************

# Base Types and Functions, InputOutput adapters, and original versions of things
echo "copy Base Types and Functions, InputOutput adapters..."
cp $srcPath/BaseFunctions/target/scala-2.11/basefunctions_2.11-0.1.0.jar $KAMANJALIBPATH
cp $srcPath/BaseTypes/target/scala-2.11/basetypes_2.11-0.1.0.jar $KAMANJALIBPATH
cp $srcPath/InputOutputAdapters/FileSimpleInputOutputAdapters/target/scala-2.11/filesimpleinputoutputadapters_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/InputOutputAdapters/KafkaSimpleInputOutputAdapters/target/scala-2.11/kafkasimpleinputoutputadapters_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/EnvContexts/SimpleEnvContextImpl/target/scala-2.11/simpleenvcontextimpl_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/MetadataBootstrap/Bootstrap/target/scala-2.11/bootstrap_2.11-1.0.jar $KAMANJALIBPATH

# Storage jars
echo "copy Storage jars..."
cp $srcPath/Storage/target/scala-2.11/storage_2.11-0.0.0.2.jar $KAMANJALIBPATH

# Metadata jars
echo "copy Metadata jars..."
cp $srcPath/Metadata/target/scala-2.11/metadata_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/MessageDef/target/scala-2.11/messagedef_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/MetadataAPI/target/scala-2.11/metadataapi_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/Pmml/MethodExtractor/target/scala-2.11/methodextractor_2.11-1.0.jar $KAMANJALIBPATH

# Kamanja jars
echo "copy Kamanja jars..."
cp $srcPath/KamanjaBase/target/scala-2.11/kamanjabase_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/KamanjaManager/target/scala-2.11/kamanjamanager_2.11-1.0.jar $KAMANJALIBPATH

# Pmml compile and runtime jars
echo "copy Pmml compile and runtime jars..."
cp $srcPath/Pmml/PmmlRuntime/target/scala-2.11/pmmlruntime_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/Pmml/PmmlUdfs/target/scala-2.11/pmmludfs_2.11-1.0.jar $KAMANJALIBPATH
cp $srcPath/Pmml/PmmlCompiler/target/scala-2.11/pmmlcompiler_2.11-1.0.jar $KAMANJALIBPATH

# sample configs
#echo "copy sample configs..."
cp $srcPath/Utils/KVInit/src/main/resources/*cfg $KAMANJALIBPATH

# other jars 
echo "copy other jars..."
cp $srcPath/../externals/log4j/log4j-1.2.17.jar $KAMANJALIBPATH
cp $srcPath/Utils/Serialize/target/scala-2.11/serialize_2.11-1.0.jar $KAMANJALIBPATH

# env context jars
echo "env context jars..."
cp $HOME/.ivy2/cache/asm/asm/jars/asm-3.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.codahale.metrics/metrics-core/bundles/metrics-core-3.0.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.yammer.metrics/metrics-core/jars/metrics-core-2.2.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.datastax.cassandra/cassandra-driver-core/bundles/cassandra-driver-core-2.0.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.esotericsoftware.kryo/kryo/bundles/kryo-2.21.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.esotericsoftware.minlog/minlog/jars/minlog-1.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.esotericsoftware.reflectasm/reflectasm/jars/reflectasm-1.07-shaded.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.fasterxml.jackson.core/jackson-annotations/bundles/jackson-annotations-2.3.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.fasterxml.jackson.core/jackson-core/bundles/jackson-core-2.3.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.fasterxml.jackson.core/jackson-databind/bundles/jackson-databind-2.3.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.github.stephenc.findbugs/findbugs-annotations/jars/findbugs-annotations-1.3.9-1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.google.code.findbugs/jsr305/jars/jsr305-1.3.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.google.collections/google-collections/jars/google-collections-1.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.google.guava/guava/bundles/guava-16.0.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.google.protobuf/protobuf-java/bundles/protobuf-java-2.6.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.google.protobuf/protobuf-java/bundles/protobuf-java-2.6.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.jamesmurty.utils/java-xmlbuilder/jars/java-xmlbuilder-0.4.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.jcraft/jsch/jars/jsch-0.1.42.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.ning/compress-lzf/bundles/compress-lzf-0.9.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.novocode/junit-interface/jars/junit-interface-0.11-RC1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.sleepycat/je/jars/je-4.0.92.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.sun.jersey/jersey-core/bundles/jersey-core-1.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.sun.jersey/jersey-json/bundles/jersey-json-1.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.sun.jersey/jersey-server/bundles/jersey-server-1.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.sun.xml.bind/jaxb-impl/jars/jaxb-impl-2.2.3-1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.twitter/chill-java/jars/chill-java-0.5.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.twitter/chill_2.11/jars/chill_2.11-0.5.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-beanutils/commons-beanutils-core/jars/commons-beanutils-core-1.8.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-beanutils/commons-beanutils/jars/commons-beanutils-1.7.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-cli/commons-cli/jars/commons-cli-1.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-codec/commons-codec/jars/commons-codec-1.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-collections/commons-collections/jars/commons-collections-3.2.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-configuration/commons-configuration/jars/commons-configuration-1.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-dbcp/commons-dbcp/jars/commons-dbcp-1.4.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-digester/commons-digester/jars/commons-digester-1.8.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-el/commons-el/jars/commons-el-1.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-httpclient/commons-httpclient/jars/commons-httpclient-3.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-io/commons-io/jars/commons-io-2.4.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-lang/commons-lang/jars/commons-lang-2.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-logging/commons-logging/jars/commons-logging-1.1.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-net/commons-net/jars/commons-net-3.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-pool/commons-pool/jars/commons-pool-1.5.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/io.netty/netty/bundles/netty-3.9.0.Final.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/javax.activation/activation/jars/activation-1.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/javax.servlet.jsp/jsp-api/jars/jsp-api-2.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/javax.servlet/servlet-api/jars/servlet-api-2.5.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/javax.xml.bind/jaxb-api/jars/jaxb-api-2.2.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/javax.xml.stream/stax-api/jars/stax-api-1.0-2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/jline/jline/jars/jline-0.9.94.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/joda-time/joda-time/jars/joda-time-2.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/junit/junit/jars/junit-4.11.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/log4j/log4j/bundles/log4j-1.2.17.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/net.java.dev.jets3t/jets3t/jars/jets3t-0.9.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/net.java.dev.jna/jna/jars/jna-3.2.7.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.avro/avro/jars/avro-1.7.4.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.commons/commons-compress/jars/commons-compress-1.4.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.commons/commons-math3/jars/commons-math3-3.1.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hadoop/hadoop-annotations/jars/hadoop-annotations-2.4.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hadoop/hadoop-auth/jars/hadoop-auth-2.4.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hadoop/hadoop-common/jars/hadoop-common-2.4.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hbase/hbase-client/jars/hbase-client-0.98.4-hadoop2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hbase/hbase-common/jars/hbase-common-0.98.4-hadoop2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.hbase/hbase-protocol/jars/hbase-protocol-0.98.4-hadoop2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.httpcomponents/httpclient/jars/httpclient-4.2.5.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.httpcomponents/httpcore/jars/httpcore-4.2.4.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.zookeeper/zookeeper/jars/zookeeper-3.4.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.cloudera.htrace/htrace-core/jars/htrace-core-2.04.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.codehaus.jackson/jackson-core-asl/jars/jackson-core-asl-1.8.8.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.codehaus.jackson/jackson-jaxrs/jars/jackson-jaxrs-1.8.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.codehaus.jackson/jackson-mapper-asl/jars/jackson-mapper-asl-1.8.8.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.codehaus.jackson/jackson-xc/jars/jackson-xc-1.8.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.codehaus.jettison/jettison/bundles/jettison-1.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.hamcrest/hamcrest-core/jars/hamcrest-core-1.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.jdom/jdom/jars/jdom-1.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.joda/joda-convert/jars/joda-convert-1.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.json4s/json4s-ast_2.11/jars/json4s-ast_2.11-3.2.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.json4s/json4s-core_2.11/jars/json4s-core_2.11-3.2.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.json4s/json4s-jackson_2.11/jars/json4s-jackson_2.11-3.2.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.json4s/json4s-native_2.11/jars/json4s-native_2.11-3.2.9.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.mapdb/mapdb/bundles/mapdb-1.0.6.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.mortbay.jetty/jetty-util/jars/jetty-util-6.1.26.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.mortbay.jetty/jetty/jars/jetty-6.1.26.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.objenesis/objenesis/jars/objenesis-1.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.ow2.asm/asm-commons/jars/asm-commons-4.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.ow2.asm/asm-tree/jars/asm-tree-4.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.ow2.asm/asm/jars/asm-4.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.scala-lang/scalap/jars/scalap-2.11.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.scala-sbt/test-interface/jars/test-interface-1.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.scalameta/quasiquotes_2.11/jars/quasiquotes_2.11-0.0.3.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.scalatest/scalatest_2.11/bundles/scalatest_2.11-2.2.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.slf4j/slf4j-api/jars/slf4j-api-1.7.7.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.slf4j/slf4j-log4j12/jars/slf4j-log4j12-1.7.5.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.tukaani/xz/jars/xz-1.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.xerial.snappy/snappy-java/bundles/snappy-java-1.0.4.1.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/tomcat/jasper-compiler/jars/jasper-compiler-5.5.23.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/tomcat/jasper-runtime/jars/jasper-runtime-5.5.23.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/voldemort/voldemort/jars/voldemort-0.96.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/xmlenc/xmlenc/jars/xmlenc-0.52.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/commons-pool/commons-pool/jars/commons-pool-1.5.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.twitter/chill_2.11/jars/chill_2.11-0.5.0.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/org.apache.kafka/kafka_2.11/jars/*.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/net.sf.jopt-simple/jopt-simple/jars/jopt-simple-3.2.jar $KAMANJALIBPATH
cp $HOME/.ivy2/cache/com.101tec/zkclient/jars/zkclient-0.3.jar $KAMANJALIBPATH


# *******************************
# COPD messages data prep
# *******************************

# Prepare test messages and copy them into place

echo "Prepare test messages and copy them into place..."
cd $srcPath/Utils/KVInit/src/main/resources
gzip -c beneficiaries.csv > beneficiaries.gz
gzip -c messages_new_format.csv > messages_new_format.gz
gzip -c messages_old_format.csv > messages_old_format.gz
gzip -c messages_new_format_all.csv > messages_new_format_all.csv.gz
gzip -c messages50_2014_BIOH.csv > messages50_2014_BIOH.csv.gz

cp *gz $KAMANJALIBPATH/msgdata/

# *******************************
# All that is left is to run the KamanjaManager
# *******************************

# no debug
# java -jar $KAMANJALIBPATH/KamanjaManager-1.0 --config /tmp/KamanjaInstall/COPD.cfg

# debug version intended for eclipse attached debugging
# java -Xdebug -Xrunjdwp:transport=dt_socket,address=8998,server=y -jar $KAMANJALIBPATH/KamanjaManager-1.0 --config /tmp/KamanjaInstall/COPD.cfg


echo "installKamanja complete..."
