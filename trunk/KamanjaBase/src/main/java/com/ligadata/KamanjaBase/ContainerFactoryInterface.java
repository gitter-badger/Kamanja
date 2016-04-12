package com.ligadata.KamanjaBase;

public interface ContainerFactoryInterface extends ContainerOrConceptFactory {
	enum ContainerType {
		MESSAGE, CONTAINER;
	}

	public abstract boolean hasPrimaryKey();

	public abstract boolean hasPartitionKey();

	public abstract boolean hasTimePartitionInfo();

	public abstract TimePartitionInfo getTimePartitionInfo();

	public abstract ContainerType getContainerType(); // ContainerType is enum of MESSAGE or CONTAINER

	public abstract boolean isFixed();

	public abstract String getAvroSchema();

	public abstract String[] getPrimaryKeyNames();

	public abstract String[] getPartitionKeyNames();

	public abstract ContainerInterface createInstance();

	public abstract int getSchemaId();
}
