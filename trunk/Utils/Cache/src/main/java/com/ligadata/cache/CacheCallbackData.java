package com.ligadata.cache;

public class CacheCallbackData {
    CacheCallbackData() {
        this.eventType = "";
        this.key = "";
        this.value = "";
    }
    CacheCallbackData(String eventType, String key, String value) {
        this.eventType = eventType;
        this.key = key;
        this.value = value;
    }
    public String eventType;
    public String key;
    public String value;
}
