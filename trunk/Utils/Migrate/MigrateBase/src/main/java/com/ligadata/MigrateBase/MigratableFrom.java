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

public interface MigratableFrom {
  public abstract void init(String srouceInstallPath, String metadataStoreInfo, String dataStoreInfo, String statusStoreInfo);
  public abstract boolean isInitialized();
  public abstract TableName[] getAllMetadataTableNames();
  public abstract TableName[] getAllDataTableNames();
  public abstract TableName[] getAllStatusTableNames();
  public abstract void getAllMetadataObjs(String backupTblSufix, MetadataObjectCallBack callbackFunction, String[] excludeMetadata);
  public abstract void getAllDataObjs(String backupTblSufix, MetadataFormat[] orderMetadata, DataObjectCallBack callbackFunction);
  public abstract void shutdown();
}

