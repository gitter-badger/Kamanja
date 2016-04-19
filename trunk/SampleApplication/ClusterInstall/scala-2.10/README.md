*******************************************
Kamanja Install and Administration Scripts 
*******************************************

The current trunk/SampleApplication/ClusterInstall scripts should be copied to a directory on your PATH.  Verify they are executable and if not perform the appropriate 'chmod' command on them.  

The NodeInfoExtract-1.0 application, used by the KamanjaClusterInstall.sh script, also must be on your PATH.  It can be found in the trunk/Utils/NodeInfoExtract/target/scala-2.10 directory.

In addition if you plan to use the RecentLogErrorsFromKamanjaCluster.sh script, the DateCalc-1.0 application should also be on your path.  It is used to calculate a timestamp some seconds or minutes or hours ago that will be used determine the first record and those more recent to examine for ERRORs.  Its project is in trunk/Utils/DateCalc.

*******************************************
Pre-requisites before using the installer 
*******************************************

See <some wiki> for some details regarding what software is expected to be available and configured appropriately.

***********************************
Install a New Cluster (from source)
***********************************

1) Run the install script without arguments to see what is required.  For example,

	rich@pepper:~/github/Kamanja/trunk$ KamanjaClusterInstall.sh

	Problem: Incorrect number of arguments

	Usage if building from source:
	      KamanjaClusterInstall.sh --ClusterId <cluster name identifer> 
	                               --MetadataAPIConfig  <metadataAPICfgPath>  
	                               --KafkaInstallPath <kafka location>
	                               --NodeConfigPath <engine config path> 
	                               [ --WorkingDir <alt working dir>  ]
	Usage if deploying tarball:
	      KamanjaClusterInstall.sh --ClusterId <cluster name identifer> 
	                               --MetadataAPIConfig  <metadataAPICfgPath>  
	                               --TarballPath <tarball path>
	                               --NodeConfigPath <engine config path>
	                               [ --WorkingDir <alt working dir>  ]

	  NOTES: Only tar'd gzip files are supported for the tarballs at the moment.
	         The NodeConfigPath must be supplied always.  
	         The working directory, by default, is /tmp.  
	         If such a public location is abhorrent, chose a private one.  It
	         must be an existing directory and readable by this script, however
	         If both the KafkaInstallPath and the TarballPath are specified, the script fails.
	         If neither the KafkaInstallPath or TarballPath  is supplied, the script will fail. 
	         A ClusterId is a required argument.  It will use only the nodes identified with that cluster id.

	         In addition, the NodeInfoExtract application that is used by this installer to fetch cluster node configuration  
	         information must be on the PATH.  It is found in the trunk/Utils/NodeInfoExtract/target/scala-2.10/ 

That is the message you will get.  As you can see, there were insufficient arguments submitted.  What will be required for building the cluster from source will be a ClusterId of one of the cluster's defined in the metadata cache OR if that has not yet been accomplished, one can specifty the cluster configuration json file to use that describes the cluster.  

Since this is a source build, the kafka installation must be specified.  This is required by the easyinstaller script that is used to build a single instance of the installation on the local (build) machine.  Unless you are particularly paranoid, the WorkingDir parameter may be omitted as it defaults to /tmp, available on all linux boxes.

Important: the build must be run from the trunk of the local repo that is to be built and deployed with the installer.

2) About the MetadataAPIConfig file

For the installation, the metadata configuration identifies some very important parameters needed by the installer. The primary one is the installation directory where all installations will be placed on ALL machines in the cluster node specification used.  It also identifies the location of the metadata cache that contains node configuration information for the cluster being installed.  

Note that when the NodeConfigPath is not specified, the installer expects to find a Cluster declaration in the metadata located with the MetadataSchemaName, MetadataStoreType, and MetadataLocation values found in this file.  For example, if the cluster to be installed is going to use a metadata cache based upon Cassandra, the parameters in the file would look like:

	MetadataSchemaName=metadata
	MetadataStoreType=cassandra
	MetadataLocation=localhost

Clearly the location could be specified as something other than localhost.  You can use an ip4 address there that locates the cassandra data store.  For Cassandra installations that run on a cluster, just choose one of the ips for now.  We may allow multiple ip addresses for the Metadata location in the future. For example:

	MetadataSchemaName=metadata
	MetadataStoreType=cassandra
	MetadataLocation=192.168.200.216

The easy installer project has several sample template configuration files, including one that uses HashMap, appropriate for simple test clusters, and one that uses Cassandra where the metadata is assumed to be on the localhost.  See trunk/SampleApplication//EasyInstall/template/config "properties" files ... ClusterCfgMetadataAPIConfig_ Cassandra _ Template.properties and ClusterCfgMetadataAPIConfig_Template.properties for examples.  You could take a copy of one of these and fill it with the appropriate location information for your MetadataAPIConfig parameter.

3) About the NodeConfigPath file

While there may be some installations that can predefine the cluster config in a shared Metadata cache, for this installer, the node config is supplied and expected not to be present in the Metadata cache.  

There is an example in trunk/SampleApplication//EasyInstall/template/config/ClusterConfig _ Template.json.  Like the metadata configuration file, a copy of this could be made and configured for a NodeConfigPath parameter.  If this is done, the substutions done for {InstallDirectory} in particular need to match your ROOT_DIR parameter in the MetadataAPIConfig.  There are other {variables} that need to have legitimate values for the nodes participating in your installation as well, like {ScalaInstallDirectory} and {JavaInstallDirectory}.  

4) About the KafkaInstallPath parameter

When building from source, the location of your local build machine kafka installation is required because the cluster installer uses the easyInstallKamanja.sh script which needs it to configure CreateQueues.sh, WatchOutputQueue.sh, WatchStatusQueue.sh and WatchInputQueue.sh.  

Note: This points out that when multiple machines are to be used for the cluster installation, ALL machines must have the Kafka installed in the same location.  

5) About WorkingDir

This defaults to /tmp.  Unless you are particularly paranoid that others with access to the machines to receive the installation will foul your installation by tampering with the files in this public location, it does not need to be specified.

6) An example invocation of the KamanjaClusterInstall.sh:

	rich@pepper:~/github/Kamanja/trunk$ KamanjaClusterInstall.sh --ClusterId ligadata1 --MetadataAPIConfig ClusterCfgMetadataAPIConfigCass.properties --KafkaInstallPath ~/tarballs/kafka/2.10/kafka_2.10-0.8.1.1 --NodeConfigPath ClusterConfig.json


************************************
Install a New Cluster (from tarball)
************************************

1) Let's focus in on the parameters (snipped from the prior section) that deal with tarball installation:

	Usage if deploying tarball:
	      KamanjaClusterInstall.sh --ClusterId <cluster name identifer> 
	                               --MetadataAPIConfig  <metadataAPICfgPath>  
	                               --TarballPath <tarball path>
	                               --NodeConfigPath <engine config path>
	                               [ --WorkingDir <alt working dir>  ]

All of the parameter descriptions common with the "source" installation in the previous section are sufficiently explained there.  Refer to those descriptions.  The only new one is the TarballPath.  It will be considered in the next several points.

2) Why use a tarball installation?

Depending on the nature of the organization deploying Kamanja clusters, the team that designs and creates the Kamanja cluster may not necessarily be the one that deploys the cluster.  Similarly, the team that deploys the cluster does not have permissions and/or knowledge to build the cluster software from source.  In these cases, a tarball installation is recommended.

3) The basic steps to create the tarball are:

	a) Run the easyInstallKamanja.sh script on the build machine.  This will create a local installation at the specified <install dir> location.  See the easyInstallKamanja.sh documentation for more details.

	b) Currently only tar'd and gzip'd tarballs are supported.  To create the "tgz" archive for your <install dir> content, change directory to <install dir>'s parent directory and issue 'tar cvzf someTarbBallName.tar.gz <install dir>'.  This tar.gz archive's file path will be the file to specify for the TarballPath value.

	c) In practice, you will want to supply some meaningful information in the tarball name like the release tag from your repo's build, a timestamp, etc.

4) Once created, invoke your KamanjaClusterInstall.sh command.  For example,

	rich@pepper:~/github/Kamanja/trunk$ KamanjaClusterInstall.sh --ClusterId ligadata1 --MetadataAPIConfig ClusterCfgMetadataAPIConfigCass.properties --TarballPath ~/kamanjaBuilds/someTarbBallName.tar.gz --NodeConfigPath ClusterConfig.json

******************
What can go wrong?  
******************

1) Short answer: lots.  Read on for an enumeration of some of the most common issues.

2) Your cluster configuration must refer to valid ip addresses that are reachable from the build/deploy machine.  The paths in the configuration files need to be legitimateand paths.

3) The script uses scp and ssh liberally to make the installation.  The script expects a "passphrase-less" access to the machines in the target cluster declaration, including the local node if it is to participate in the installation.  If this is not the case, there will be errors when the tarball that is created from source (or supplied on the command line) is copied to the password protected node.  See <some wiki page> for how to correct this and what is expected for Kamanja installations in terms of ssh configuration and access.

4) If you expect to have success with the Kamanja cluster install script, it is important to use the same directory topology for your critical components.  These include kafka, zookeeper, cassandra, hbase, scala, and java to name a few.

5) Parameters supplied to the KamanjaClusterInstall.sh need to be cogent.  There are roughly 15 semantic checks currently done on the parameters to the script.  If any problems are detected, the script stops and a meaningful message is written to the console.  They are for the most part self-explanatory, describing with some precision what the problem is.

6) "I tried to run the cluster after installing it, but an error was thrown."  It is important to realize that the installer does not configure your cluster installation or the applications that are to run on it.  That work must be done after (or if using HBase or Cassandra, before or after) the cluster is installed.  The KamanjaClusterInstall.sh's job is to push the installation to the appropriate machines based upon the cluster configuration given to it via parameters.  Nothing more is done.

7) Installing the correct versions of the software needed to build and run the Kamanja cluster is exceedingly important.  For example, certain libraries upon which Kamanja depends prevent us from simply using the latest versions of the Scala compiler.  Always check <some wiki page> for what the requirements are for the version you wish to install.

8) The NodeConfigPath is both your friend and your enemy.  If you supply one, it had better NOT be defined in your metadata cache already.  This is principally a problem only when metadata is maintained in Cassandra or HBase.  There will be errors and the installation will fail.  It is intended principally for testing builds and doing development work (typically with a TreeMap or HashMap metadata store... see <some wiki page> for more information about that).  A mature Kamanja installation will likely already have the cluster declaration installed in the cache before the cluster installation takes place, making the NodeConfigPath unnecessary.

9) This list will be augmented with additional issues as this software is experienced by others and they report them.

10) The script can't find NodeInfoExtract-1.0.  What should I do?  Be sure that this application is on your PATH.  See the "Install the KamanjaClusterInstall installer" section above for more details.

*******************************************
Before Starting a Newly Installed Kamanja Cluster
*******************************************

Once the installation of the cluster is accomplished, it is necessary to add all application metadata to the Metadata cache.
The installation doesn't install any application messages, containers, types, function defs, or models as part of the installation.  That is left to do with explicit interaction with the MetadataAPI service.

See <the appropriate MetadataAPI user wiki reference> for more information.

*******************************************
Start a KamanjaCluster
*******************************************

To start a cluster is a bit simpler than the install script.  There are two familiar arguments.  Running the StartKamanjaCluster.sh with no arguments produces this:

Problem: Incorrect number of arguments

	Usage:
	      StartKamanjaCluster.sh --ClusterId <cluster name identifer> 
	                           --MetadataAPIConfig  <metadataAPICfgPath> 

	  NOTES: Start the cluster specified by the cluster identifier parameter.  Use the metadata api configuration to locate
	         the appropriate metadata store.  

	The ClusterId is the string identifier for the cluster.  It should refer to legitimate cluster metadata in the Metadata cache found in the MetadataAPIConfig file given.

The script will read the cluster metadata, contact each of the nodes described by it, and start the Kamanja engine at that location.  A Process Identifier (PID) is recorded and written to the installation directory's run directory for each of the nodes started.  This PID file will be used by the StatusKamanjaCluster.sh script to verify that a process is alive on each respective cluster node.  The StopKamanjaCluster.sh script uses the PID to stop respective cluster nodes that are running.


*******************************************
Stop a KamanjaCluster
*******************************************

The stop cluster script takes the same arguments as the start script.  Running the StopKamanjaCluster.sh script without arguments produces:

	Problem: Incorrect number of arguments

	Usage:
	      StopKamanjaCluster.sh --ClusterId <cluster name identifer> 
	                           --MetadataAPIConfig  <metadataAPICfgPath>  

	  NOTES: Stop the cluster specified by the cluster identifier parameter.  Use the metadata api configuration to locate
	         the appropriate metadata store.  

Like start, the stop script contacts each node in the cluster and stops the process with the PID value that was written by the start script.

*******************************************
Getting Status a KamanjaCluster
*******************************************

To see if nodes of a cluster are alive, one can use the StatusKamanjaCluster.sh.  Again, there are two familiar arguments.  Running the StatusKamanjaCluster.sh with no arguments produces this:

	Problem: Incorrect number of arguments


	Usage:
	      StatusKamanjaCluster.sh --ClusterId <cluster name identifer> 
	                           --MetadataAPIConfig  <metadataAPICfgPath>  

	  NOTES: Get status on the cluster specified by the cluster identifier parameter.  Use the metadata api 
	  		configuration to locate the appropriate metadata store.   

The ClusterId is the string identifier for the cluster.  It should refer to legitimate cluster metadata in the Metadata cache found in the MetadataAPIConfig file given.

The script will read the cluster metadata, contact each of the nodes described by it, retrieve the respective PIDs and issue a ps command to see the status of the Kamanja engine that is running.  The script will report the health of each node (whether it is up or down).

*******************************************
Findout What is Wrong - Using Kamanja Log tools
*******************************************

In an ideal world, there are never problems with the software running on the system.  The software to be described in this section is not for that utopian view.  It is to find out why things are going bad.  The "devil is in the details" they say, and for Kamanja, the details are found in the system logs on each node in a given cluster.

You have two log scraper choices.  Both look for '- ERROR -' messages in the logs on a given cluster.  

1) The first of the two is called LogErrorsFromKamanjaCluster.sh.  It will search an entire log.  Running it without arguments tells what is required for arguments:

	Problem: Incorrect number of arguments

	Answer any errors from the Kamanja cluster log

	Usage:
	      LogErrorsFromKamanjaCluster.sh --ClusterId <cluster name identifer> 
	                                     --MetadataAPIConfig  <metadataAPICfgPath>  
	                                     --KamanjaLogPath <kamanja system log path>
	                                     [--ErrLogPath <where errors are collected> ] 

	  NOTES: Logs for the cluster specified by the cluster identifier parameter found in the metadata api 
	         configuration.  
	         Default error log path is "/tmp/errorLog.log" .. errors collected in this file 


If your practice is to roll logs every hour, this is probably the script for you.  The error log path will receive just the error lines found in the log.  Because the system log can be moved about with log4j configuration options, the script insists that you give the location of the Kamanja logs.  As written, only the current log will be searched.  If one were to schedule a job that runs every five minutes, the script will nominally run 12 times before log roll over.  The errors of course would repeatedly be emitted for each of the runs during the hour, however, this is satisfactory behvavior for simple console dash board applications.

Note that all nodes errors are logged to the error log on the administation machine that has issued the script.  Output currently looks like this:

	Node 1 (Errors detected at 2015-04-17 21:16:47)  :
	file /tmp/drdigital/logs/testlog.log not found
	No ERRORs found for this period
	Node 2 (Errors detected at 2015-04-17 21:16:47) :
	2015-04-17 21:12:44,467 - com.ligadata.MetadataAPI.MetadataAPIImpl$ - ERROR - Closing datastore failed
	2015-04-17 23:22:41,484 - com.ligadata.MetadataAPI.MetadataAPIImpl$ - ERROR - metdatastore is corrupt
	2015-04-17 24:02:14,493 - com.ligadata.MetadataAPI.MetadataAPIImpl$ - ERROR - transStore died
	2015-04-17 24:12:34,500 - com.ligadata.MetadataAPI.MetadataAPIImpl$ - ERROR - jarStore has no beans
	2015-04-17 24:22:54,508 - com.ligadata.MetadataAPI.MetadataAPIImpl$ - ERROR - configStore hammered


In this example, there is no log found for Node 1.  Node 2 has had a very bad 4 minutes.

2) The second option is the script, RecentLogErrorsFromKamanjaCluster.sh.  This script produces the same sort of output at the other.  It, however, is designed not to read the entire log.  Instead the script invocation can be configured to only examine log records written to the log in the last "InLast" units, where the unit can be minute, second, hour, or day.

Running without arguments shows the semantics:

	Problem: Incorrect number of arguments

	Answer any errors from the Kamanja cluster in the past N time units

	Usage:
	      RecentLogErrorsFromKamanjaCluster.sh --ClusterId <cluster name identifer> 
	                                           --MetadataAPIConfig  <metadataAPICfgPath>  
	                                           --InLast <unit count>
	                                           --KamanjaLogPath <kamanja system log path>
	                                          [--ErrLogPath <where errors are collected> ] 
	                                          [--Unit <time unit ... any of {minute, second, hour, day}> ] 

	  NOTES: Start the cluster specified by the cluster identifier parameter.  Use the metadata api 
	         configuration to locate the appropriate metadata store.  
	         Default time unit is "minute". 
	         Default error log path is "/tmp/errorLog.log" .. errors collected in this file 

One might choose this script over the other for any of the following reasons:

1) the Kamanja clusters is heavily used with lots of transactions both in terms of metadata and model processing traffic
2) the high volume dictates more frequent log queries for the admin screen updates.
3) the log rolling is dictated by logs reaching a substantial size before rolling; this would either make log scanning prohibitively expensive or cause too much output to be provided to the admin screen to be manageable to monitor (if not both).

*************************
Configuration Issues

If possible, use the simple full log scanner.  If your organization policy and/or Kamanja use is substantial and you think you need the second log scanner, it is recommended that the LOG_TICK=1 be set in your engine startup configuration template.  This will guarantee that each log in the cluster will get a "tick" record each second such that there is no activity on that cluster node.  This guarantees that the simple csplit log '/beginning time/' regular expression will find a record to split upon.

A better scanner that is more sophisticated than csplit is planned.

