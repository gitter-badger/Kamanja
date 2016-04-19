
NOTES: 
	1. Make sure we have upto date {NewPackageInstallPath}/config/ClusterConfig.json & {NewPackageInstallPath}/config/MetadataAPIConfig.properties in {NewPackageInstallPath} Path
	2. Make sure we have source version information & destination version information properly set in MigrateConfig.json. Otherwise it will corrupt the whole database.

Command to execute after Updating {NewPackageInstallPath} with the NewPackageInstalledPath in {NewPackageInstallPath}/Migration/MigrateConfig.json
java -Dlog4j.configurationFile=file:{NewPackageInstallPath}/Migrate/log4j2.xml -jar {NewPackageInstallPath}/Migrate/MigrateManager-1.0 --config {NewPackageInstallPath}/Migrate/MigrateConfig.json

