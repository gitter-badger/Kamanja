##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**
##**Produce model results for ''Iris dataset'' models using jpmml-evaluator's test tool**
##**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<**

##**The expectation is that you download the latest jpmml-evalutator from github and build it with maven.**
##
##**For example,
##
##**mvn clean install**
##
##**This will create the test application jar that can consume a single model and single input file (see commands below)**


##**You must set these Env variables before invoking tests**
export JPMML_TEST_BASE=/home/rich/github/dev/jpmml-evaluator/pmml-evaluator-example
export KAMANJA_SRCDIR=/home/rich/github/dev/jpmml/kamanja/trunk
cd $KAMANJA_SRCDIR


##**KNIME models**

Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/DecisionTreeEnsembleIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/DecisionTreeEnsembleIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/GeneralRegressionIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/GeneralRegressionIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/KMeansIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/KMeansIris.tsv
#Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/single_iris_dectree.xml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/single_iris_dectree.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/DecisionTreeIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/DecisionTreeIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/KMeansEnsembleIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/KMeansEnsembleIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/NeuralNetworkIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/NeuralNetworkIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/KNIME/SupportVectorMachineIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/KNIME/SupportVectorMachineIris.tsv

##**RapidMiner models**

Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/RapidMiner/DecisionTreeIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/RapidMiner/DecisionTreeIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/RapidMiner/KMeansIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/RapidMiner/KMeansIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/RapidMiner/NeuralNetworkIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/RapidMiner/NeuralNetworkIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/RapidMiner/RuleSetIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/RapidMiner/RuleSetIris.tsv

##**R/Rattle models**

Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/DecisionTreeIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/DecisionTreeIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/KernlabSVMIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/KernlabSVMIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/LibSVMIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/LibSVMIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/NeuralNetworkIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/NeuralNetworkIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/RandomForestXformIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/RandomForestXformIris.tsv
#Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/decision_tree_iris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/decision_tree_iris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/KMeansIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/KMeansIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/LogisticRegressionIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/LogisticRegressionIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/RandomForestIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv  >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/RandomForestIris.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/HierarchicalClusteringIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/HierarchicalClusteringIris.tsv
#Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/k_means_iris_pmml.xml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/k_means_iris_pmml.tsv
Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/NaiveBayesIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/NaiveBayesIris.tsv
#Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Rattle/random_forest_iris_pmml.xml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Rattle/random_forest_iris_pmml.tsv

##**SAS models**

Pmml/JpmmlAdapter/src/main/resources/bin/pmml-evaluator-test.sh $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/metadata/model/Sas/LogisticRegressionIris.pmml $KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/data/iris.raw.csv >$KAMANJA_SRCDIR/Pmml/JpmmlAdapter/src/main/resources/jpmmlResults/Sas/LogisticRegressionIris.tsv
