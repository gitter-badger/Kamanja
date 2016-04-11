package com.ligadata.KamanjaBase;

//FIXME:: Need to change ContainerOrConcept name to some meaningful name

public abstract class ContainerOrConcept {
    ContainerOrConceptFactory factory = null;

    public ContainerOrConcept(ContainerOrConceptFactory factory) {
        this.factory = factory;
    }

    final public ContainerOrConceptFactory getFactory() {
        return factory;
    }

    final public String getFullTypeName() {
        return factory.getFullTypeName();
    }

    final public String getTypeNameSpace() {
        return factory.getTypeNameSpace();
    }

    final public String getTypeName() {
        return factory.getTypeName();
    }

    final public String getTypeVersion() {
        return factory.getTypeVersion();
    }
}
