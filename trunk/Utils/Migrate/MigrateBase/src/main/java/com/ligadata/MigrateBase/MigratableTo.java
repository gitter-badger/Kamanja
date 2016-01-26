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

package com.ligadata.MigrateBase;

public interface MigratableTo {
  public abstract void init(String destInstallPath, String apiConfigFile, String clusterConfigFile, String sourceVersion, String unhandledMetadataDumpDir, String curMigrationSummaryFlPath); // Source version is like 1.1 or 1.2, etc
  public abstract boolean isInitialized();
  public abstract String getMetadataStoreInfo();
  public abstract String getDataStoreInfo();
  public abstract String getStatusStoreInfo();
  public abstract boolean isMetadataTableExists(TableName tblInfo);
  public abstract boolean isDataTableExists(TableName tblInfo);
  public abstract boolean isStatusTableExists(TableName tblInfo);
  public abstract void backupMetadataTables(BackupTableInfo[] tblsToBackedUp, boolean force);
  public abstract void backupDataTables(BackupTableInfo[] tblsToBackedUp, boolean force);
  public abstract void backupStatusTables(BackupTableInfo[] tblsToBackedUp, boolean force);
  public abstract void dropMetadataTables(TableName[] tblsToDrop);
  public abstract void dropDataTables(TableName[] tblsToDrop);
  public abstract void dropStatusTables(TableName[] tblsToDrop);
  public abstract void dropMessageContainerTablesFromMetadata(MetadataFormat[] allMetadataElemsJson);
  public abstract void addMetadata(MetadataFormat[] allMetadataElemsJson, boolean uploadClusterConfig, String[] excludeMetadata);
  public abstract void populateAndSaveData(DataFormat[] data);
  public abstract void shutdown();
}

