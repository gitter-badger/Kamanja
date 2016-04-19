package com.ligadata.KamanjaBase;

//FIXME:: Decide whether we need to generate concept class for every concept or use generic class like this
public class Concept extends ContainerOrConcept {
	String key = null;
	String valueType = null;
	Object value = null;

	public Concept(ConceptFactory factory) {
		super(factory);
	}

	public Concept(ConceptFactory factory, String key, String valueType, Object value) {
		super(factory);
		this.key = key;
		this.valueType = valueType;
		this.value = value;
	}
}