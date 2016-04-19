package com.ligadata.KamanjaBase;

public class AttributeValue {

	AttributeTypeInfo valueType;
	Object value = "";
	
	public AttributeValue(){
		
	}
	public AttributeValue(Object value, AttributeTypeInfo valueType) {		
		this.value = value;
		this.valueType = valueType;
	}

	public AttributeTypeInfo getValueType() {
		return valueType;
	}

	public void setValueType(AttributeTypeInfo valueType) {
		this.valueType = valueType;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
	}	
}
