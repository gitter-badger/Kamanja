package com.ligadata.cache;

import java.util.Map;

/**
 * Created by Saleh on 3/15/2016.
 */
public interface DataCache {
    public void init(String jsonString);
    public void start();
    public void put(String key, Object value);
    public void put(Map map);
    public Object get(String key);
    public Map<String, Object> get(String[] keys);
    public void shutdown();
}
