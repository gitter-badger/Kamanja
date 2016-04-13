package com.ligadata.KamanjaBase;

import java.io.DataInputStream;
import java.io.DataOutputStream;

public abstract class MessageContainerBase extends ContainerOrConcept {

	long transactionId = 0;
	long timePartitionData = 0;
	int rowNumber = 0;

	public MessageContainerBase(MessageContainerObjBase factory) {
		super(factory);
	}

	final ContainerFactoryInterface getContainerFactory() {
		return ((ContainerFactoryInterface) getFactory());
	}

	final public long getTransactionId() {
		return this.transactionId;
	}

	final public long getTimePartitionData() {
		return this.timePartitionData;
	}

	final public int getRowNumber() {
		return this.rowNumber;
	}

	final public int getSchemaId() {
		return getContainerFactory().getSchemaId();
	}

	final public ContainerTypes.ContainerType getContainerType() {
		return getContainerFactory().getContainerType();
	}

	final public boolean isFixed() {
		return getContainerFactory().isFixed();
	}

	final public String getAvroSchema() {
		return getContainerFactory().getAvroSchema();
	}

	final public String[] getPrimaryKeyNames() {
		return getContainerFactory().getPrimaryKeyNames();
	}

	final public String[] getPartitionKeyNames() {
		return getContainerFactory().getPartitionKeyNames();
	}

	final public TimePartitionInfo getTimePartitionInfo() {
		return getContainerFactory().getTimePartitionInfo();
	}

	final public void setTransactionId(long transId) {
		this.transactionId = transId;
	}

	final public void setTimePartitionData(long timePrtData) {
		this.timePartitionData = timePrtData;
	}

	final public void setRowNumber(int rowNbr) {
		this.rowNumber = rowNbr;
	}

	public abstract String[] getPrimaryKey();

	public abstract String[] getPartitionKey();

	public abstract void save();

	public abstract String[] getAttributeNames();

	public abstract AttributeTypeInfo[] getAttributeTypes();

	public abstract AttributeTypeInfo getAttributeType(String name);

	public abstract Object get(int index); // Return (value, type)

	public abstract Object getOrElse(int index, Object defaultVal); // Return
																	// (value,
																	// type)

	public abstract AttributeValue[] getAllAttributeValues(); // Has (name,
																// value, type))

	public abstract void set(int index, Object value);

	public abstract void set(String key, Object value, String valTyp);

	public abstract java.util.Iterator<AttributeValue> getAttributeNameAndValueIterator();

	public void TransactionId(long transId) {
		this.transactionId = transId;
	}

	public long TransactionId() {
		return this.transactionId;
	}

	public void TimePartitionData(long timeInMillisecs) {
		timePartitionData = timeInMillisecs;
	}

	public long TimePartitionData() {
		return timePartitionData;
	}

	public void RowNumber(int rno) {
		rowNumber = rno;
	}

	public int RowNumber() {
		return rowNumber;
	}

	final public boolean hasPrimaryKey() {
		return getContainerFactory().hasPrimaryKey();
	}

	final public boolean hasPartitionKey() {
		return getContainerFactory().hasPartitionKey();
	}

	final public boolean hasTimePartitionInfo() {
		return getContainerFactory().hasTimePartitionInfo();
	}

	public abstract Object get(String key); // Return (value, type)

	public abstract Object getOrElse(String key, Object defaultVal); // Return
																		// value
																		// type

	public abstract void set(String key, Object value);

	final public boolean isMessage() {
		return (getContainerFactory().getContainerType() == ContainerTypes.ContainerType.MESSAGE);
	}

	public boolean isContainer() {
		return (getContainerFactory().getContainerType() == ContainerTypes.ContainerType.CONTAINER);
	}

	final public boolean IsFixed() {
		return getContainerFactory().isFixed();
	}

	final public boolean IsKv() {
		return getContainerFactory().IsKv();
	}

	final public boolean CanPersist() {
		return getContainerFactory().CanPersist();

	}

	final public void populate(InputData inputdata) throws Exception {
		throw new Exception(
				"Populate method is deprecated and is not supported any more");
	}

	final public String Version() {
		return getContainerFactory().getTypeVersion();
	}

	final public String[] PartitionKeyData() {
		return getPartitionKey();
	}

	final public String[] PrimaryKeyData() {
		return getPrimaryKey();
	}

	final public String FullName() {
		return getContainerFactory().FullName();
	}

	final public String NameSpace() {
		return getContainerFactory().NameSpace();
	}

	final public String Name() {
		return getContainerFactory().Name();
	}

	final public void Deserialize(DataInputStream dis,
			MdBaseResolveInfo mdResolver, java.lang.ClassLoader loader,
			String savedDataVersion) throws Exception {
		throw new Exception(
				"Populate method is deprecated and is not supported any more");
	}

	final public void Serialize(DataOutputStream dos) throws Exception {
		throw new Exception(
				"Populate method is deprecated and is not supported any more");
	}

	final public void Save() {
		this.save();
	}

	public abstract ContainerOrConcept Clone();

	/*
	 * def AddMessage(childPath: Array[(String, String)], msg: BaseMsg): Unit
	 * def GetMessage(childPath: Array[(String, String)], primaryKey:
	 * Array[String]): BaseMsg def getNativeKeyValues:
	 * scala.collection.immutable.Map[String, (String, Any)]
	 */

}
