##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**General SETUP**
##**1) initialize Kamanja home (KAMANJA_HOME) and the repo trunk directory (KAMANJA_SRCDIR)**
##**2) Debug flag can be supplied on command line now as first argument to the kamanja command.**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Env setup**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/Sprint8FeaturesWithJpmml/kamanja/trunk
cd $KAMANJA_HOME

##**Env setup**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/jpmml/kamanja/trunk
cd $KAMANJA_HOME


##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Instructions for namespace tests.**
##**1. Add the rel10 messages for the medical app.**
##**2. Add the copdv3 model, the udf types, the udf functions, and the (rel20) message, beneficiaryV1.json**
##**3. Add the copdv4 model that uses both the rel10 and rel20 message (plus the rel10 containers).  This demonstrates**
##**the NameSpaceSearchPath usage... specifically how the search path can be used to find types and functions first found style and**
##**by explicitly qualifying messages and other objects to defeat the default search path**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**


##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**(rel10) com/medco/messages and containers <<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Add config, messages and containers**

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties upload cluster config $KAMANJA_HOME/config/ClusterConfig.json

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/container/CoughCodes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/container/DyspnoeaCodes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/container/EnvCodes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/container/SmokeCodes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/container/SputumCodes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/message/beneficiary.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/message/hl7.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/message/inpatientclaim.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/message/outpatientclaim.json

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**(rel10) Kamanja Pmml model<<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**Add copdv3**
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/model/COPDv3.xml

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**(rel10) Kamanja Pmml Udfs<<<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**Add new types and functions for the custom udfs needed to build the message, beneficiaryV1.json and the model that uses it, COPDv4.xml**
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties load types from a file $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/type/udfTypes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties load functions from a file $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/function/udfFcns.json

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**(rel20) Kamanja Pmml Message and Model <<<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**Add the message, beneficiaryV1.json and the model that uses it, COPDv4.xml**
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/message/beneficiaryV1.json

##**Add the COPDv4.xml**
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/nmspcMsgContainerTest/metadata/model/COPDv4.xml

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Instructions for pmmludfTests (dateFcnTests, macroTests, missingValueRepl)**
##**1. Add the rel10 messages for the medical app... some of the test cases are hijacked med xml modified to illustrate/test some feature**
##**2. Add unit test messages**
##**3. Add the udf functions**
##**5. Add the Models dateFcnTests.xml  macroTests.xml  missingValueRepl.xml**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**medical system messages and containers <<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties upload cluster config $KAMANJA_HOME/config/ClusterConfig.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_HOME/input/SampleApplications/metadata/container/CoughCodes_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_HOME/input/SampleApplications/metadata/container/DyspnoeaCodes_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_HOME/input/SampleApplications/metadata/container/EnvCodes_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_HOME/input/SampleApplications/metadata/container/SmokeCodes_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_HOME/input/SampleApplications/metadata/container/SputumCodes_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_HOME/input/SampleApplications/metadata/message/beneficiary_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_HOME/input/SampleApplications/metadata/message/hl7_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_HOME/input/SampleApplications/metadata/message/inpatientclaim_Medical.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_HOME/input/SampleApplications/metadata/message/outpatientclaim_Medical.json

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**system messages and containers <<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/container/hl7history.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/container/inpatientclaimhistory.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/container/macroStateSimple.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add container $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/container/outpatientclaimhistory.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/message/twitter_msg.json

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties getallmessages
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties getallcontainers

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Types and Udfs**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties load types from a file $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/type/CustomUdfTypes.json
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties load functions from a file $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/function/CustomUdfFcns.json

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Kamanja Pmml Models - dateFcnTests.xml  macroTests.xml  missingValueRepl.xml**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/model/dateFcnTests.xml
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/model/macroTests.xml
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/metadata/model/missingValueRepl.xml

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model kpmml $KAMANJA_HOME/input/SampleApplications/metadata/model/COPDRiskAssessmentv2_Medical.xml

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**


##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Kamanja Native Models - COPDRiskAssessment.java  COPDRiskAssessment.scala**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties upload compile config $KAMANJA_HOME/config/COPDRiskAssessmentCompileCfg.json

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model java $KAMANJA_HOME/input/SampleApplications/metadata/model/COPDRiskAssessment.java DEPENDSON copdriskassessment
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add model scala $KAMANJA_HOME/input/SampleApplications/metadata/model/COPDRiskAssessment.scala DEPENDSON copdriskassessment

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**ENGINE TESTING SETUP**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**init kv store for medical**
$KAMANJA_HOME/input/SampleApplications/bin/InitKvStores_Medical.sh

##**Establish queues**
$KAMANJA_HOME/bin/CreateQueues.sh --partitions 1

##**watch output console**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/Sprint8FeaturesWithJpmml/kamanja/trunk
cd $KAMANJA_HOME
$KAMANJA_HOME/bin/WatchOutputQueue.sh

##**start engine console**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/Sprint8FeaturesWithJpmml/kamanja/trunk
cd $KAMANJA_HOME
$KAMANJA_HOME/bin/StartEngine.sh
##**StartEngine.sh debug**



##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**TEST Medical Apps (java, scala, and pmml) and unit tests (macroTests, missingValueRepl)**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Env setup**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/Sprint8FeaturesWithJpmml/kamanja/trunk
cd $KAMANJA_HOME

##**push data console**
#export useCompressed=0
export useCompressed=1
$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/bin/JpmmlPushKafka.sh $useCompressed $KAMANJA_HOME/input/SampleApplications/data/copd_demo_Medical.csv.gz

##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**TEST Apps (dateFcnTests)**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Env setup**
export KAMANJA_HOME=/tmp/drdigital
export KAMANJA_SRCDIR=/home/rich/github/dev/Sprint8FeaturesWithJpmml/kamanja/trunk
cd $KAMANJA_HOME

##**push data console**
#export useCompressed=0
export useCompressed=1
$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/bin/JpmmlPushKafka.sh $useCompressed $KAMANJA_SRCDIR/Pmml/PmmlUdfs/src/test/resources/pmmludfTests/data/pump1.csv.gz
