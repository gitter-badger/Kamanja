This project contains a scala script that will accept a project name, full package qualified object, a namespace, a version number and file paths for the types and function defs that are to be produced.  These files are sent to the MetadataAPI to be cataloged for use in the PMML models.

This script should be placed in the executable path (i.e., PATH).  When executing the script, it is expected that the
current working directory is the top level sbt project directory (e.g., ~/github/Kamanja/trunk).

Here is a more complete explanation of the command semantics:

extractUdfLibMetadata.scala --sbtProject <projectName> 
                            --fullObjectPath <full pkg qualifed object name> 
                            --namespace <namespace to use when generating JSON fcn objects> 
                            --versionNumber <numeric version to use>
                            --typeDefsPath <types file path>
                            --fcnDefsPath <function definition file path>
        where sbtProject is the sbt project name (presumably with udfs in one of its object definitions)
              fullObjectPath is the object that contains the methods for which to generate the json metadata
              namespace is the namespace to use for the udfs in the object's generated json
              versionNumber is the version number to use for the udfs in the object's generated json
              typeDefsPath is the file path that will receive any type definitions that may be needed to catalog the functions
                 being collected
              fcnDefsPath is the file path that will receive the function definitions
      
        Notes: This script must run from the top level sbt project directory (e.g., ~/github/Kamanja/trunk)
        The object argument supplied must inherit from com.ligadata.pmml.udfs.UdfBase for extraction to be successful.
        Obviously it is helpful if the project actually builds.  This script executes the fat jar version of the 
        MethodExtractor (<sbtRoot>/trunk/Pmml/MethodExtractor/target/scala-2.10/MethodExtractor-1.0)

        Only one full object path may be specified in this version.
      


Example - creating the typedefs and functiondefs in use by the core library (PmmlUdfs):

	extractUdfLibMetadata.scala --sbtProject PmmlUdfs --fullObjectPath com.ligadata.pmml.udfs.Udfs --namespace pmml --versionNumber 100 --typeDefsPath /tmp/json.types.txt --fcnDefsPath /tmp/json.fcn.txt

The json.types.txt file contains any typedefs that this lib needs.  The json.fcn.txt file contains the function definitions.  As the names suggests, these are in JSON format.  They are sent to the <product name> cluster with the MetadataAPIService.

