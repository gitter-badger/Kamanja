package com.ligadata.kamanja.CheckerComponent;

public class ComponentInfo {
	String Version;        // version
	String Status;         // success/fail
	String ErrorMessage;   // include stack trace also if it has
	// String invocationNode; // node from where it got invoke
	String ComponentName;  // zookeeper/hbase/kafka/java/scala

/*
	public String getInvocationNode() {
		return invocationNode;
	}
	public void setInvocationNode(String invocationNode) {
		this.invocationNode = invocationNode;
	}
*/

	public String getComponentName() {
		return ComponentName;
	}
	public void setComponentName(String componentName) {
		ComponentName = componentName;
	}
	public String getVersion() {
		return Version;
	}
	public void setVersion(String version) {
		Version = version;
	}
	public String getStatus() {
		return Status;
	}
	public void setStatus(String status) {
		Status = status;
	}
	public String getErrorMessage() {
		return ErrorMessage;
	}
	public void setErrorMessage(String errorMessage) {
		ErrorMessage = errorMessage;
	}
}
