/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.Migrate;

import org.apache.logging.log4j.*;

import java.io.File;

import com.ligadata.MigrateBase.*;

import java.util.*;
import java.lang.reflect.Constructor;
// import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import com.google.gson.Gson;
// import com.google.gson.GsonBuilder;
import java.io.FileReader;

public class Migrate {
	String loggerName = this.getClass().getName();
	Logger logger = LogManager.getLogger(loggerName);

	class VersionConfig {
		String version = null;
		String versionInstallPath = null;
		String implemtedClass = null;
		List<String> jars = null;

		/*
		 * VersionConfig(String tversion, String tversionInstallPath, String
		 * timplemtedClass, List<String> tjars) { version = tversion;
		 * versionInstallPath = tversionInstallPath; implemtedClass =
		 * timplemtedClass; jars = tjars; }
		 */
		VersionConfig() {
		}
	}

	class Configuration {
		int dataSaveThreshold = 1000;
		String clusterConfigFile = null;
		String apiConfigFile = null;
		String unhandledMetadataDumpDir = null;
		VersionConfig migratingFrom = null;
		VersionConfig migratingTo = null;
		List<String> excludeMetadata = null;
		boolean excludeData = false;

		/*
		 * Configuration(String tclusterConfigFile, String tapiConfigFile,
		 * VersionConfig tmigratingFrom, VersionConfig tmigratingTo) {
		 * clusterConfigFile = tclusterConfigFile; apiConfigFile =
		 * tapiConfigFile; migratingFrom = tmigratingFrom; migratingTo =
		 * tmigratingTo; }
		 */
		Configuration() {
		}
	}

	List<MetadataFormat> allMetadata = new ArrayList<MetadataFormat>();

	class DataCallback implements DataObjectCallBack {
		long cntr = 0;
		MigratableTo migrateTo = null;
		List<DataFormat> collectedData = null;
		int kSaveThreshold = 0;
		String srcVer = "0";
		String dstVer = "0";
		byte[] appendData = new byte[0];

		DataCallback(MigratableTo tmigrateTo, List<DataFormat> tcollectedData,
				int tkSaveThreshold, String tsrcVer, String tdstVer) {
			migrateTo = tmigrateTo;
			collectedData = tcollectedData;
			kSaveThreshold = tkSaveThreshold;
			srcVer = tsrcVer;
			dstVer = tdstVer;

			if (srcVer.equalsIgnoreCase("1.1")
					&& dstVer.equalsIgnoreCase("1.3")) {
				appendData = new byte[8]; // timepartition bytes at the end.
				for (int i = 0; i < 8; i++)
					appendData[0] = 0;
			}
		}

		public boolean call(DataFormat[] objData) throws Exception {
			for (DataFormat d : objData) {
				int readLen = d.data.length;

				if ((d.containerName.equalsIgnoreCase("AdapterUniqKvData"))
						|| (d.containerName.equalsIgnoreCase("ModelResults"))) {
					// Don't change any data
				} else {
					if (appendData.length > 0) {
						byte[] result = new byte[d.data.length
								+ appendData.length];
						System.arraycopy(d.data, 0, result, 0, d.data.length);
						System.arraycopy(appendData, 0, result, d.data.length,
								appendData.length);
						d.data = result;
					}
				}

				int writeLen = d.data.length;
				logger.debug(String
						.format("cntr:%d, Container:%s, TimePartitionValue:%d, BucketKey:%s, TxnId:%d, RowId:%d, SerializerName:%s, DataSize:(Read:%d, Write:%d)",
								cntr, d.containerName, d.timePartition,
								Arrays.toString(d.bucketKey), d.transactionid,
								d.rowid, d.serializername, readLen, writeLen));
				collectedData.add(d);
				cntr += 1;
			}
			if (collectedData.size() >= kSaveThreshold) {
				migrateTo.populateAndSaveData(collectedData
						.toArray(new DataFormat[collectedData.size()]));
				collectedData.clear();
			}
			return true;
		}
	}

	class MdCallback implements MetadataObjectCallBack {
		public boolean call(MetadataFormat objData) throws Exception {
			logger.debug(String.format("Got Metadata => Key:%s, JsonString:%s",
					objData.objType, objData.objDataInJson));
			allMetadata.add(objData);
			return true;
		}
	}

	Configuration GetConfigurationFromCfgFile(String cfgfile) {
		FileReader reader = null;
		try {
			reader = new FileReader(cfgfile);
			try {
				Gson gson = new Gson();
				Configuration cfg = gson.fromJson(reader, Configuration.class);
				logger.debug("Populated migrate configuration:"
						+ gson.toJson(cfg));
				return cfg;
			} catch (Exception e) {
			} catch (Throwable e) {
			} finally {

			}
			return null;
		} catch (Exception e) {
		} catch (Throwable e) {
		} finally {
			try {
				if (reader != null)
					reader.close();
			} catch (Exception e) {
			}
		}
		return null;
	}

	void usage() {
		logger.warn("Usage: migrate --config <ConfigurationJsonFile>");
	}

	boolean isValidPath(String path, boolean checkForDir, boolean checkForFile,
			String str) {
		File fl = new File(path);
		if (fl.exists() == false) {
			logger.error("Given " + str + ":" + path + " does not exists");
			return false;
		}

		if (checkForDir && fl.isDirectory() == false) {
			logger.error("Given " + str + ":" + path + " is not directory");
			return false;
		}

		if (checkForFile && fl.isFile() == false) {
			logger.error("Given " + str + ":" + path + " is not file");
			return false;
		}

		return true;
	}

	public void run(String[] args) {
		MigratableFrom migrateFrom = null;
		MigratableTo migrateTo = null;
		URLClassLoader srcKamanjaLoader = null;
		URLClassLoader dstKamanjaLoader = null;

		try {
			if (args.length != 2) {
				usage();
				return;
			}

			String backupTblSufix = "_migrate_bak";

			String cfgfile = "";
			if (args[0].equalsIgnoreCase("--config")) {
				cfgfile = args[1].trim();
			} else {
				logger.error("Unknown option " + args[0]);
				usage();
				System.exit(1);
			}

			if (cfgfile.length() == 0) {
				logger.error("Input required config file");
				usage();
				System.exit(1);
			}

			if (isValidPath(cfgfile, false, true, "ConfigFile") == false) {
				usage();
				System.exit(1);
			}

			Configuration configuration = GetConfigurationFromCfgFile(cfgfile);

			if (configuration == null) {
				logger.error("Failed to get configuration from given file:"
						+ cfgfile);
				usage();
				System.exit(1);
			}

			// Version check

			String srcVer = configuration.migratingFrom.version.trim();
			String dstVer = configuration.migratingTo.version.trim();

			if (srcVer.equalsIgnoreCase("1.1") == false
					&& srcVer.equalsIgnoreCase("1.2") == false) {
				logger.error("We support source versions only 1.1 or 1.2. We don't support "
						+ srcVer);
				usage();
				System.exit(1);
			}

			if (dstVer.equalsIgnoreCase("1.3") == false) {
				logger.error("We support destination version only 1.3. We don't support "
						+ srcVer);
				usage();
				System.exit(1);
			}

			String curMigrationUnhandledMetadataDumpDir = "";
			String curMigrationSummary = "";

			if (isValidPath(configuration.unhandledMetadataDumpDir, true,
					false, "unhandledMetadataDumpDir")) {
				String dirExtn = new java.text.SimpleDateFormat(
						"yyyyMMddHHmmss").format(new java.util.Date(System
						.currentTimeMillis()));

				String newDir = configuration.unhandledMetadataDumpDir
						+ "/Migrate_" + dirExtn;

				File fl = new File(newDir);

				if (fl.mkdir()) {
					curMigrationUnhandledMetadataDumpDir = fl.getAbsolutePath();
					curMigrationSummary = configuration.unhandledMetadataDumpDir
							+ "/MigrateSummary_" + dirExtn + ".log";
					logger.info("Current Migration Changes are writing into "
							+ curMigrationUnhandledMetadataDumpDir
							+ " directory. And summary is writing into "
							+ curMigrationSummary);
				} else {
					logger.error("Failed to create directory " + newDir);
					usage();
					System.exit(1);
				}
			} else {
				usage();
				System.exit(1);
			}

			// From Srouce version 1.1 to Destination version 1.3 we do both
			// Metadata Upgrade & Data Upgrade
			// From Source Version 1.2 to Destination version 1.3, we only do
			// Metadata Upgrade.
			boolean canUpgradeMetadata = ((srcVer.equalsIgnoreCase("1.1") || srcVer
					.equalsIgnoreCase("1.2")) && dstVer.equalsIgnoreCase("1.3"));
			boolean canUpgradeData = (srcVer.equalsIgnoreCase("1.1") && dstVer
					.equalsIgnoreCase("1.3"));

			if (canUpgradeData && canUpgradeMetadata == false) {
				logger.error("We don't support upgrading only data without metadata at this moment");
				usage();
				System.exit(1);
			}

			// Modify canUpgradeData depending on exclude flag
			canUpgradeData = (canUpgradeData && configuration.excludeData == false);

			int srcJarsCnt = configuration.migratingFrom.jars.size();
			URL[] srcLoaderUrls = new URL[srcJarsCnt];

			int idx = 0;
			for (String jar : configuration.migratingFrom.jars) {
				logger.debug("Migration From URL => " + jar);
				srcLoaderUrls[idx++] = new File(jar).toURI().toURL();
			}

			srcKamanjaLoader = new URLClassLoader(srcLoaderUrls);

			Class<?> srcClass = srcKamanjaLoader
					.loadClass(configuration.migratingFrom.implemtedClass);

			Constructor<?> srcConstructor = srcClass.getConstructor();
			Object tmpSrcObj = srcConstructor.newInstance();
			if (tmpSrcObj instanceof MigratableFrom) {
				migrateFrom = (MigratableFrom) tmpSrcObj;
			} else {
				logger.error(String
						.format("Failed to Load Source. Version:%s, migrateFromClass:%s, InstallPath:%s, ",
								configuration.migratingFrom.version,
								configuration.migratingFrom.implemtedClass,
								configuration.migratingFrom.versionInstallPath));
			}

			int dstJarsCnt = configuration.migratingTo.jars.size();
			URL[] dstLoaderUrls = new URL[dstJarsCnt];

			idx = 0;
			for (String jar : configuration.migratingTo.jars) {
				logger.debug("Migration To URL => " + jar);
				dstLoaderUrls[idx++] = new File(jar).toURI().toURL();
			}

			dstKamanjaLoader = new URLClassLoader(dstLoaderUrls);

			Class<?> dstClass = dstKamanjaLoader
					.loadClass(configuration.migratingTo.implemtedClass);

			Constructor<?> dstConstructor = dstClass.getConstructor();
			Object tmpDstObj = dstConstructor.newInstance();
			if (tmpDstObj instanceof MigratableTo) {
				migrateTo = (MigratableTo) tmpDstObj;
			} else {
				logger.error(String
						.format("Failed to Load Destination. Version:%s, migrateToClass:%s",
								configuration.migratingTo.version,
								configuration.migratingTo.implemtedClass));
			}

			logger.debug(String.format(
					"apiConfigFile:%s, clusterConfigFile:%s",
					configuration.apiConfigFile,
					configuration.clusterConfigFile));
			migrateTo.init(configuration.migratingTo.versionInstallPath,
					configuration.apiConfigFile,
					configuration.clusterConfigFile, srcVer,
					curMigrationUnhandledMetadataDumpDir, curMigrationSummary);

			String metadataStoreInfo = migrateTo.getMetadataStoreInfo();
			String dataStoreInfo = migrateTo.getDataStoreInfo();
			String statusStoreInfo = migrateTo.getStatusStoreInfo();

			logger.debug(String
					.format("metadataStoreInfo:%s, dataStoreInfo:%s, statusStoreInfo:%s",
							metadataStoreInfo, dataStoreInfo, statusStoreInfo));
			migrateFrom.init(configuration.migratingFrom.versionInstallPath,
					metadataStoreInfo, dataStoreInfo, statusStoreInfo);

			TableName[] allMetadataTbls = new TableName[0];
			TableName[] allDataTbls = new TableName[0];
			TableName[] allStatusTbls = new TableName[0];

			if (canUpgradeMetadata)
				allMetadataTbls = migrateFrom.getAllMetadataTableNames();
			if (canUpgradeData) {
				allDataTbls = migrateFrom.getAllDataTableNames();
				allStatusTbls = migrateFrom.getAllStatusTableNames();
			}

			List<BackupTableInfo> metadataBackupTbls = new ArrayList<BackupTableInfo>();
			List<BackupTableInfo> dataBackupTbls = new ArrayList<BackupTableInfo>();
			List<BackupTableInfo> statusBackupTbls = new ArrayList<BackupTableInfo>();

			List<TableName> metadataDelTbls = new ArrayList<TableName>();
			List<TableName> dataDelTbls = new ArrayList<TableName>();
			List<TableName> statusDelTbls = new ArrayList<TableName>();

			boolean allTblsBackedUp = true;

			for (TableName tblInfo : allMetadataTbls) {
				BackupTableInfo bkup = new BackupTableInfo(tblInfo.namespace,
						tblInfo.name, tblInfo.name + backupTblSufix);

				if (migrateTo.isMetadataTableExists(tblInfo)) {
					if (migrateTo.isMetadataTableExists(new TableName(
							tblInfo.namespace, bkup.dstTable)) == false) {
						allTblsBackedUp = false;
					}
					metadataBackupTbls.add(bkup);
					metadataDelTbls.add(tblInfo);
				}
			}

			for (TableName tblInfo : allDataTbls) {
				BackupTableInfo bkup = new BackupTableInfo(tblInfo.namespace,
						tblInfo.name, tblInfo.name + backupTblSufix);
				if (migrateTo.isDataTableExists(tblInfo)) {
					if (migrateTo.isDataTableExists(new TableName(
							tblInfo.namespace, bkup.dstTable)) == false) {
						allTblsBackedUp = false;
					}
					dataBackupTbls.add(bkup);
					dataDelTbls.add(tblInfo);
				}

			}

			for (TableName tblInfo : allStatusTbls) {
				BackupTableInfo bkup = new BackupTableInfo(tblInfo.namespace,
						tblInfo.name, tblInfo.name + backupTblSufix);
				if (migrateTo.isStatusTableExists(tblInfo)) {
					if (migrateTo.isStatusTableExists(new TableName(
							tblInfo.namespace, bkup.dstTable)) == false) {
						allTblsBackedUp = false;
					}
					statusBackupTbls.add(bkup);
					statusDelTbls.add(tblInfo);
				}
			}

			// Backup all the tables, if any one of them is missing
			if (allTblsBackedUp == false) {
				if (metadataBackupTbls.size() > 0)
					migrateTo.backupMetadataTables(metadataBackupTbls
							.toArray(new BackupTableInfo[metadataBackupTbls
									.size()]), true);
				if (dataBackupTbls.size() > 0)
					migrateTo.backupDataTables(
							dataBackupTbls
									.toArray(new BackupTableInfo[dataBackupTbls
											.size()]), true);
				if (statusBackupTbls.size() > 0)
					migrateTo.backupStatusTables(statusBackupTbls
							.toArray(new BackupTableInfo[statusBackupTbls
									.size()]), true);
			}

			// Drop all tables after backup
			if (metadataDelTbls.size() > 0)
				migrateTo.dropMetadataTables(metadataDelTbls
						.toArray(new TableName[metadataDelTbls.size()]));
			if (dataDelTbls.size() > 0)
				migrateTo.dropDataTables(dataDelTbls
						.toArray(new TableName[dataDelTbls.size()]));
			if (statusDelTbls.size() > 0)
				migrateTo.dropStatusTables(statusDelTbls
						.toArray(new TableName[statusDelTbls.size()]));

			String[] excludeMetadata = new String[0];
			if (configuration.excludeMetadata != null
					&& configuration.excludeMetadata.size() > 0) {
				excludeMetadata = configuration.excludeMetadata
						.toArray((new String[configuration.excludeMetadata
								.size()]));
			}

			if (canUpgradeMetadata) {
				migrateFrom.getAllMetadataObjs(backupTblSufix,
						new MdCallback(), excludeMetadata);
			}

			MetadataFormat[] metadataArr = allMetadata
					.toArray(new MetadataFormat[allMetadata.size()]);

			if (canUpgradeData)
				migrateTo.dropMessageContainerTablesFromMetadata(metadataArr);

			if (canUpgradeMetadata)
				migrateTo.addMetadata(metadataArr, true, excludeMetadata);

			if (canUpgradeData) {
				int kSaveThreshold = 1000;

				if (configuration.dataSaveThreshold > 0)
					kSaveThreshold = configuration.dataSaveThreshold;

				List<DataFormat> collectedData = new ArrayList<DataFormat>();

				DataCallback dataCallback = new DataCallback(migrateTo,
						collectedData, kSaveThreshold, srcVer, dstVer);

				migrateFrom.getAllDataObjs(backupTblSufix, metadataArr,
						dataCallback);

				if (collectedData.size() > 0) {
					migrateTo.populateAndSaveData(collectedData
							.toArray(new DataFormat[collectedData.size()]));
					collectedData.clear();
				}
			}

			logger.info("Migration is done");
			System.out.println("Migration is done");
		} catch (Exception e) {
			logger.error("Failed to Migrate", e);
		} catch (Throwable t) {
			logger.error("Failed to Migrate", t);
		} finally {
			if (migrateFrom != null)
				migrateFrom.shutdown();
			if (migrateTo != null)
				migrateTo.shutdown();
			try {
				if (srcKamanjaLoader != null)
					srcKamanjaLoader.close();
			} catch (Exception e) {
			} catch (Throwable t) {
			}

			try {
				if (dstKamanjaLoader != null)
					dstKamanjaLoader.close();
			} catch (Exception e) {
			} catch (Throwable t) {
			}
		}
	}

	public static void main(String[] args) {
		new Migrate().run(args);
	}
}
