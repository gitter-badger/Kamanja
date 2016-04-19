package com.ligadata.KamanjaBase;

public class TimePartitionInfo {

	String fieldName = "";
	String format = "";
	TimePartitionType timePartitionType; // types - Daily, Monthly, Yearly

	public enum TimePartitionType {
		DAILY, MONTHLY, YEARLY, NONE;
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String format) {
		this.format = format;
	}

	public TimePartitionType getTimePartitionType() {
		return timePartitionType;
	}

	public void setTimePartitionType(TimePartitionType timePartitionType) {
		this.timePartitionType = timePartitionType;
	}

}
