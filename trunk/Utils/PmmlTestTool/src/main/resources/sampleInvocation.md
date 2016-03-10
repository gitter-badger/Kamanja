
export KAMANJA_HOME=/tmp/drdigital/Kamanja-1.3.3_2.11
export ipport=8998
export KAMANJA_SRCDIR=/home/rich/github/dev/dev1.3/kamanja/trunk

java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:$KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/log4j2.xml -jar $KAMANJA_SRCDIR/Utils/PmmlTestTool/target/PmmlTestTool-1.0 --pmmlSrc $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/metadata/model/clus_m1_Rattle_Audit_PMML.xml --dataset $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/data/Audit.csv --omitInputs


java -Xdebug -Xrunjdwp:transport=dt_socket,address="$ipport",server=y -Dlog4j.configurationFile=file:$KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/log4j2.xml -jar $KAMANJA_SRCDIR/Utils/PmmlTestTool/target/PmmlTestTool-1.0 --pmmlSrc $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/metadata/model/clus_m1_Rattle_Audit_PMML.xml --dataset $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/data/Audit.sansColumnQuotes.csv --omitInputs

$KAMANJA_SRCDIR/Utils/PmmlTestTool/target/PmmlTestTool-1.0 --pmmlSrc $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/metadata/model/clus_m1_Rattle_Audit_PMML.xml --dataset $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/data/Audit.csv --omitInputs

$KAMANJA_SRCDIR/Utils/PmmlTestTool/target/PmmlTestTool-1.0 --pmmlSrc $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/metadata/model/clus_m1_Rattle_Audit_PMML.xml --dataset $KAMANJA_SRCDIR/Utils/PmmlTestTool/src/main/resources/data/Audit.sansColumnQuotes.csv --omitInputs

