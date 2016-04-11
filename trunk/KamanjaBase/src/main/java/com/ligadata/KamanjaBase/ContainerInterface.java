package com.ligadata.KamanjaBase;

public abstract class ContainerInterface extends ContainerOrConcept {
    long transactionId = 0;
    long timePartitionData = 0;
    int rowNumber = 0;

    public ContainerInterface(ContainerFactoryInterface factory) {
        super(factory);
    }

    final ContainerFactoryInterface getContainerFactory() {
        return ((ContainerFactoryInterface)getFactory());
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

    final public long getTransactionId() {
        return transactionId;
    }

    final public long getTimePartitionData() {
        return timePartitionData;
    }

    final public int getRowNumber() {
        return rowNumber;
    }

    final public int getSchemaId() {
        return getContainerFactory().getSchemaId();
    }

    final public ContainerFactoryInterface.ContainerType getContainerType() {
        return getContainerFactory().getContainerType();
    }

    final public boolean isFixed() {
        return getContainerFactory().isFixed();
    }

    final public String getSchema() {
        return getContainerFactory().getSchema();
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

    public abstract ContainerOrConcept Clone();

    public abstract String[] getAttributeNames();

    public abstract AttributeValue get(String key); // Return (value, type)

    public abstract AttributeValue get(int index); // Return (value, type)

    public abstract AttributeValue getOrElse(String key, Object defaultVal); // Return (value, type)

    public abstract AttributeValue getOrElse(int index, Object defaultVal); // Return (value,  type)

    public abstract java.util.HashMap<String, AttributeValue> getAllAttributeValues(); // Has (name, value, type))

    public abstract void set(String key, Object value);

    public abstract void set(int index, Object value);
    
    public abstract void set(String key, Object value, String valTyp) ;
    
    public abstract java.util.Iterator<java.util.Map.Entry<String, AttributeValue>> getAttributeNameAndValueIterator();
}
