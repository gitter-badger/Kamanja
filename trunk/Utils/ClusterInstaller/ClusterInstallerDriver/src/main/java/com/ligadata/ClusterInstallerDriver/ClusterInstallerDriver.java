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

package com.ligadata.ClusterInstallerDriver;

import com.ligadata.InstallDriverBase.InstallDriverBase;
import com.ligadata.KamanjaVersion.KamanjaVersion;
import org.apache.logging.log4j.*;

import java.io.File;

// import com.ligadata.MigrateBase.*;
import com.ligadata.Migrate.*;

import java.lang.reflect.Constructor;
import java.net.URL;
import java.net.URLClassLoader;

public class ClusterInstallerDriver implements StatusCallback {
    String loggerName = this.getClass().getName();
    Logger logger = LogManager.getLogger(loggerName);
    InstallDriverBase installDriver = null;

    boolean isValidPath(String path, boolean checkForDir, boolean checkForFile) {
        File fl = new File(path);
        if (!fl.exists())
            return false;

        if (checkForDir && !fl.isDirectory())
            return false;

        if (checkForFile && !fl.isFile())
            return false;

        return true;
    }

    public void call(String statusText, String typStr) {
        if (installDriver != null)
            installDriver.statusUpdate(statusText, typStr);
    }

    void run(String[] args) {
        String thisFatJarsLocationAbs = "";
        String clusterInstallerDriversLocation = "";
        try {
            File fl = new File(getClass().getProtectionDomain().getCodeSource().getLocation().toURI());
            thisFatJarsLocationAbs = fl.getAbsolutePath();
            clusterInstallerDriversLocation = new File(fl.getParent()).getAbsolutePath();
        } catch (Exception e) {
            logger.error("Failed to get ClusterInstallerDriver Jar Absolute Path", e);
            System.out.println("Failed to get ClusterInstallerDriver Jar Absolute Path");
            System.exit(1);
        }

        logger.info("Jar Absolute Path:" + thisFatJarsLocationAbs + ", ClusterInstallerDriversLocation:" + clusterInstallerDriversLocation);
        System.out.println("Jar Absolute Path:" + thisFatJarsLocationAbs + ", ClusterInstallerDriversLocation:" + clusterInstallerDriversLocation);

        URLClassLoader installDriverLoader = null;

        int majorVer = KamanjaVersion.getMajorVersion();
        int minVer = KamanjaVersion.getMinorVersion();
        int microVer = KamanjaVersion.getMicroVersion();
        String versionStr = "" + majorVer + "." + minVer + "." + microVer;
        String versionStrJar = versionStr + ".jar";
        String installDriverJar = "InstallDriver-" + versionStr;
        String installDriverPath = clusterInstallerDriversLocation + "/" + installDriverJar;

        if (! isValidPath(installDriverPath, false, true)) {
            String msg = String.format(installDriverJar + " not found at " + installDriverPath);
            logger.error(msg);
            System.out.println(msg);
            System.exit(1);
		}

        String[] extnLibs = new String[2];
        String libsPath = clusterInstallerDriversLocation + "/../lib/system/";

        if (! isValidPath(libsPath + "ExtDependencyLibs_2.11-" + versionStrJar, false, true)) {
            if (! isValidPath(libsPath + "ExtDependencyLibs_2.10-" + versionStrJar, false, true)) {
                String msg = String.format("ExtDependencyLibs_2.11-" + versionStrJar + " or " + "ExtDependencyLibs_2.10-" + versionStrJar + " not found at " + libsPath);
                logger.error(msg);
                System.out.println(msg);
                System.exit(1);
            } else {
                extnLibs[0] = libsPath + "ExtDependencyLibs_2.10-" + versionStrJar;
            }
        } else {
            extnLibs[0] = libsPath + "ExtDependencyLibs_2.11-" + versionStrJar;
        }

        if (! isValidPath(libsPath + "ExtDependencyLibs2_2.11-" + versionStrJar, false, true)) {
            if (! isValidPath(libsPath + "ExtDependencyLibs2_2.10-" + versionStrJar, false, true)) {
                String msg = String.format("ExtDependencyLibs2_2.11-" + versionStrJar + " or " + "ExtDependencyLibs2_2.10-" + versionStrJar + " not found at " + libsPath);
                logger.error(msg);
                System.out.println(msg);
                System.exit(1);
            } else {
                extnLibs[1] = libsPath + "ExtDependencyLibs2_2.10-" + versionStrJar;
            }
        } else {
            extnLibs[1] = libsPath + "ExtDependencyLibs2_2.11-" + versionStrJar;
        }

        try {
            URL[] instDriverLoaderUrls = new URL[3];

            instDriverLoaderUrls[0] = new File(extnLibs[0]).toURI().toURL();
            instDriverLoaderUrls[1] = new File(extnLibs[1]).toURI().toURL();
            instDriverLoaderUrls[2] = new File(installDriverPath).toURI().toURL();

            installDriverLoader = new URLClassLoader(instDriverLoaderUrls);

            Class<?> instDriverClass = installDriverLoader.loadClass("com.ligadata.InstallDriver.InstallDriver");

            Constructor<?> instDriverConstructor = instDriverClass.getConstructor();
            Object tmpinstDriverObj = instDriverConstructor.newInstance();
            if (tmpinstDriverObj instanceof InstallDriverBase) {
                installDriver = (InstallDriverBase) tmpinstDriverObj;
            } else {
                String msg = String.format("Failed to Load com.ligadata.InstallDriver.InstallDriver from Jar " + installDriverPath);
                logger.error(msg);
                System.out.println(msg);
                System.exit(1);
            }

            // Call InstallDriver run
            installDriver.run(args);

            if (installDriver.migrationPending()) {
                try {
                    Migrate migrateObj = new Migrate();
                    migrateObj.registerStatusCallback(this);
                    int rc = migrateObj.runFromJsonConfigString(installDriver.migrationConfig());
                    if (rc != 0) {
                        // Failed
                        call("Some thing failed to prepare migration configuration. The parameters for the migration may be incorrect... aborting installation", "ERROR");
                        logger.error("Some thing failed to prepare migration configuration. The parameters for the migration may be incorrect... aborting installation");
                    } else {
                        // Succeeded
                        call("Processing is Complete!", "DEBUG");
                        logger.debug("Processing is Complete!");
                    }
                } catch (Exception e) {
                    logger.error("Failed to migrate with " + e.getMessage(), e);
                    call("Failed to migrate with " + e.getMessage(), "ERROR");
                } catch (Throwable t) {
                    logger.error("Failed to Migrated with " + t.getMessage(), t);
                    call("Failed to migrate with " + t.getMessage(), "ERROR");
                } finally {
                    try {
                        if (installDriverLoader != null)
                            installDriverLoader.close();
                    } catch (Exception e) {
                    } catch (Throwable t) {
                    }
                }
                installDriver.closeLog();
            }
        } catch (Exception e) {
            logger.error("ClusterInstallerDriver failed with " + e.getMessage(), e);
            call("ClusterInstallerDriver failed with " + e.getMessage(), "ERROR");
            System.exit(1);
        } catch (Throwable t) {
            logger.error("ClusterInstallerDriver failed with " + t.getMessage(), t);
            call("ClusterInstallerDriver failed with " + t.getMessage(), "ERROR");
            System.exit(1);
        } finally {
            try {
                if (installDriverLoader != null)
                    installDriverLoader.close();
            } catch (Exception e) {
            } catch (Throwable t) {
            }
        }
        return;
    }

    public static void main(String[] args) {
        ClusterInstallerDriver driver = new ClusterInstallerDriver();
        driver.run(args);
    }
}

