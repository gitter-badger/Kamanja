/*
 * Copyright 2016 ligaDATA
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

package com.ligadata.KamanjaVersion;

public class KamanjaVersion {
    static private int majorVersion = 1;
    static private int minorVersion = 4;
    static private int microVersion = 0;
    static private int buildNumber = 0;

    static public int getMajorVersion() {
        return majorVersion;
    }

    static public int getMinorVersion() {
        return minorVersion;
    }

    static public int getMicroVersion() {
        return microVersion;
    }

    static public int getBuildNumber() {
        return buildNumber;
    }

    static public String getVersionString() {
        return ("Kamanja version " + majorVersion + "." + minorVersion + "." + microVersion + "." + buildNumber);
    }

    static public String getVersionStringWithoutBuildNumber() {
        return ("Kamanja version " + majorVersion + "." + minorVersion + "." + microVersion);
    }

    static public void print() {
        System.out.println(getVersionString());
    }
}

