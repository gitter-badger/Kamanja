package com.ligadata.adapters.hdfs;

import java.io.IOException;
import java.net.URI;

import org.apache.avro.Schema;
import org.apache.avro.file.CodecFactory;
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.generic.GenericData.Record;
import org.apache.avro.generic.GenericDatumWriter;
import org.apache.avro.io.DatumWriter;
import org.apache.avro.mapred.FsInput;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.security.UserGroupInformation;
import org.apache.log4j.Logger;

public class AvroHDFSWriter {
	static Logger logger = Logger.getLogger(AvroHDFSWriter.class);
	
	private Schema schema;
	private String basePath;
	private String codec;
	private URI uri;
	private FSDataOutputStream out;
	private DataFileWriter<Record> dataFileWriter;
	private String kerberosPrincipal;
	private String keytabFile;
	private String resourceFile;

	public AvroHDFSWriter(Schema schema, String path, String codec) throws Exception {
		this.schema = schema;
		this.basePath = path;
		this.codec = codec;
		this.kerberosPrincipal = null;
		this.keytabFile = null;
		this.resourceFile = null;
	}
	
	public Schema getSchema() {
		return schema;
	}
	
	public String getKerberosPrincipal() {
		return kerberosPrincipal;
	}

	public void setKerberosPrincipal(String kerberosPrincipal) {
		this.kerberosPrincipal = kerberosPrincipal;
	}

	public String getKeytabFile() {
		return keytabFile;
	}

	public void setKeytabFile(String keytabFile) {
		this.keytabFile = keytabFile;
	}

	public String getResourceFile() {
		return resourceFile;
	}

	public void setResourceFile(String resourceFile) {
		this.resourceFile = resourceFile;
	}

	public void open(String fileName) throws IOException {
		
		uri = URI.create(basePath + "/" + fileName);
		logger.info("Opening avro writer for " + uri);
		
		Configuration conf = new Configuration();
		//conf.set("dfs.client.block.write.replace-datanode-on-failure.policy", "NEVER");
		if(resourceFile != null && !"".equals(resourceFile)) {
			logger.debug("Adding additional HDFS client configuration from file " + resourceFile);
			conf.addResource(new Path(resourceFile));
		}
		
		if(keytabFile != null && !"".equals(keytabFile) && kerberosPrincipal != null && !"".equals(kerberosPrincipal)) {
			logger.debug("Kerberos security enabled.");
			conf.set("hadoop.security.authentication", "kerberos");
			UserGroupInformation.setConfiguration(conf);

			logger.debug("Using Kerberos principal: " + kerberosPrincipal);
			logger.debug("Using Kerberos keytab file: " + keytabFile);
			UserGroupInformation.loginUserFromKeytab(kerberosPrincipal, keytabFile);
		}
		FileSystem fs = FileSystem.get(uri, conf);

		DatumWriter<Record> datumWriter = new GenericDatumWriter<Record>(schema);
		dataFileWriter = new DataFileWriter<Record>(datumWriter);
		if(codec == null || "".equals(codec)) {
			logger.info("Not using compression for avro file.");
			dataFileWriter.setCodec(CodecFactory.fromString("null"));
		} else {
			logger.info("Using " + codec + " compression for avro file.");
			dataFileWriter.setCodec(CodecFactory.fromString(codec));
		}
		
		Path path = new Path(uri);
		if (fs.exists(path)) {
			logger.info("Loading existing file " + uri);
			out = fs.append(path);
			dataFileWriter.appendTo(new FsInput(path, conf), out);
		} else {
			logger.info("Creating new file " + uri);
			out = fs.create(path);
			dataFileWriter.create(schema, out);
		}
	}
	
	public void write(Record rec) throws IOException {
		dataFileWriter.append(rec);
	}

	public void close() throws IOException {
		logger.info("Closing file at " + uri);
		if (dataFileWriter != null)
			dataFileWriter.close();
		if (out != null)
			out.close();
		
		dataFileWriter = null;
		out = null;
		uri = null;
	}
}
