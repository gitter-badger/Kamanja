package com.ligadata.KamanjaBase;

import java.text.ParseException;
import java.text.SimpleDateFormat;

public interface MessageContainerObjBase extends ContainerOrConceptFactory {
	
	public boolean hasPrimaryKey();

	public boolean hasPartitionKey();

	public boolean hasTimePartitionInfo();

	public TimePartitionInfo getTimePartitionInfo();

	public ContainerTypes.ContainerType getContainerType(); // ContainerType is enum
												// of MESSAGE or
												// CONTAINER

	public boolean isFixed();

	public String getAvroSchema();

	public String[] getPrimaryKeyNames();

	public String[] getPartitionKeyNames();

	public MessageContainerBase createInstance();

	public int getSchemaId();

	public boolean isMessage();

	public boolean isContainer();

	public boolean IsFixed();

	public boolean CanPersist();

	public String FullName();

	public boolean IsKv();

	public String NameSpace();

	public String Name();

	public String Version();

	public String[] PartitionKeyData(InputData inputdata);

	public String[] PrimaryKeyData(InputData inputdata);

	public long TimePartitionData(InputData inputdata);

	public boolean NeedToTransformData();  // Filter & Rearrange input , attributes if needed	
	
	public BaseMsg CreateNewMessage();
	
	public BaseContainer CreateNewContainer();
	// public abstract String[] getTimePartitionInfo();

	/*
	 * private long extractTime(String fieldData, String timeFormat) throws
	 * ParseException { if (fieldData == null || fieldData.trim() == "") return
	 * 0;
	 * 
	 * if (timeFormat == null || timeFormat.trim() == "") return 0;
	 * 
	 * if (timeFormat.compareToIgnoreCase("epochtimeInMillis") == 0) return
	 * Long.parseLong(fieldData.trim());
	 * 
	 * if (timeFormat.compareToIgnoreCase("epochtimeInSeconds") == 0 ||
	 * timeFormat.compareToIgnoreCase("epochtime") == 0) return
	 * Long.parseLong(fieldData.trim()) * 1000;
	 * 
	 * // Now assuming Date partition format exists. SimpleDateFormat dtFormat =
	 * new SimpleDateFormat(timeFormat);
	 * 
	 * java.util.Date tm = new java.util.Date(0); // Date tm = ; if
	 * (fieldData.length() == 0) { tm = new java.util.Date(0); } else { tm =
	 * dtFormat.parse(fieldData); } return tm.getTime(); }
	 * 
	 * public long ComputeTimePartitionData(String fieldData, String timeFormat,
	 * String timePartitionType) throws ParseException { long fldTimeDataInMs =
	 * extractTime(fieldData, timeFormat);
	 * 
	 * // Align to Partition java.util.Calendar cal =
	 * java.util.Calendar.getInstance(); cal.setTime(new
	 * java.util.Date(fldTimeDataInMs));
	 * 
	 * if (timePartitionType == null || timePartitionType.trim() == "") return
	 * 0;
	 * 
	 * switch (timePartitionType.toLowerCase()) { case "yearly": {
	 * java.util.Calendar newcal = java.util.Calendar.getInstance();
	 * newcal.setTimeInMillis(0); newcal.set(java.util.Calendar.YEAR,
	 * cal.get(java.util.Calendar.YEAR)); return newcal.getTime().getTime(); }
	 * case "monthly": { java.util.Calendar newcal =
	 * java.util.Calendar.getInstance(); newcal.setTimeInMillis(0);
	 * newcal.set(java.util.Calendar.YEAR, cal.get(java.util.Calendar.YEAR));
	 * newcal.set(java.util.Calendar.MONTH, cal.get(java.util.Calendar.MONTH));
	 * return newcal.getTime().getTime(); } case "daily": { java.util.Calendar
	 * newcal = java.util.Calendar.getInstance(); newcal.setTimeInMillis(0);
	 * newcal.set(cal.get(java.util.Calendar.YEAR),
	 * cal.get(java.util.Calendar.MONTH),
	 * cal.get(java.util.Calendar.DAY_OF_MONTH)); return
	 * newcal.getTime().getTime(); } } return 0; }
	 */
}
