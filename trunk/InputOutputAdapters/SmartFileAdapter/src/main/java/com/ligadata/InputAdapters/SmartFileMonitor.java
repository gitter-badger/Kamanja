package com.ligadata.InputAdapters;

/**
 * Created by Yasser on 3/10/2016.
 */
public interface SmartFileMonitor {
    void init(String adapterSpecificCfgJson);
    void monitor();
    void shutdown();
}
