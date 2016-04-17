package com.ligadata.KamanjaBase;

public class AttributeTypeInfo {

	String name;
	int Index;
	TypeCategory typeCategory; // from TypeCategory
	short valTypeId;
	short keyTypeId;
	long valSchemaId;

	public AttributeTypeInfo(String name, int Index, TypeCategory typeCategory,
			short valTypeId, short keyTypeId, long valSchemaId) {
		this.name = name;
		this.Index = Index;
		this.typeCategory = typeCategory;
		this.valTypeId = valTypeId;
		this.valSchemaId = valSchemaId;
		this.keyTypeId = keyTypeId;
	}

	public enum TypeCategory {
		INT(0), STRING(1), FLOAT(2), DOUBLE(3), LONG(4), BYTE(5), CHAR(6), BOOLEAN(7), CONTAINER(
				1001), MESSAGE(1001), MAP(1002), ARRAY(1003), NONE(-1);
		private int value;

		private TypeCategory(int value) {
			this.value = value;
		}

		public int getValue(){
			return this.value;
		}
	}

	public TypeCategory toTypeCategory(Short typeId) {
		switch(typeId) {
			case 0: return TypeCategory.INT;
			case 1: return TypeCategory.STRING;
			case 2: return TypeCategory.FLOAT;
			case 3: return TypeCategory.DOUBLE;
			case 4: return TypeCategory.LONG;
			case 5: return TypeCategory.BYTE;
			case 6: return TypeCategory.CHAR;
			case 7: return TypeCategory.BOOLEAN;
			case 1001: return TypeCategory.CONTAINER;
			case 1002: return TypeCategory.MESSAGE;
			case 1003: return TypeCategory.ARRAY;
			default: return TypeCategory.NONE;
		}
	}
	public boolean IsContainer() {
		return (typeCategory == TypeCategory.CONTAINER || typeCategory == TypeCategory.MESSAGE);
	}

	public boolean IsArray() {
		return (typeCategory == TypeCategory.ARRAY);
	}

	public boolean IsMap() {
		return (typeCategory == TypeCategory.MAP);
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getIndex() {
		return Index;
	}

	public void setIndex(int index) {
		Index = index;
	}

	public TypeCategory getTypeCategory() {
		return (TypeCategory) typeCategory;
	}

	public TypeCategory getKeyTypeCategory() { return toTypeCategory(keyTypeId); }

	public TypeCategory getValTypeCategory() { return toTypeCategory(valTypeId); }

	public void setTypeCategary(TypeCategory typeCategory) {
		this.typeCategory = typeCategory;
	}

	public short getValTypeId() {
		return valTypeId;
	}

	public void setValTypeId(short valTypeId) {
		this.valTypeId = valTypeId;
	}

	public short getKeyTypeId() {
		return keyTypeId;
	}

	public void setKeyTypeId(byte keyTypeId) {
		this.keyTypeId = keyTypeId;
	}

	public long getValSchemaId() {
		return valSchemaId;
	}

	public void setValSchemaId(long valSchemaId) {
		this.valSchemaId = valSchemaId;
	}

}
