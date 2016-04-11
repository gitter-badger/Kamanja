package com.ligadata.KamanjaBase;

//FIXME:: Decide whether we need to generate concept class for every concept or use generic class like this
public abstract class ConceptFactory implements ContainerOrConceptFactory {
	public abstract Concept createInstance();
}