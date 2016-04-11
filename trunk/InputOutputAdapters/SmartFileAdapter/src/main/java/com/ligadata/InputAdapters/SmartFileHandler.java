package com.ligadata.InputAdapters;

import com.ligadata.Exceptions.KamanjaException;

import java.io.InputStream;

/**
 * Created by Yasser on 3/10/2016.
 */
public interface SmartFileHandler {
    String getFullPath();
    //gets input stream based on the fs type (das/nas, hdfs, sft), usually used for file type detecting purposes
    InputStream getDefaultInputStream() throws KamanjaException;
    //prepares input stream based on the fs type and also file type itself (plain, gzip, bz2, lzo), so data can be read directly
    InputStream openForRead() throws KamanjaException;
    int read(byte[] buf, int length) throws KamanjaException;
    void close();
    boolean moveTo(String newPath) throws KamanjaException;
    boolean delete() throws KamanjaException;
    long length() throws KamanjaException;
    long lastModified() throws KamanjaException;
    boolean exists() throws KamanjaException;
    boolean isFile() throws KamanjaException;
    boolean isDirectory() throws KamanjaException;
}
