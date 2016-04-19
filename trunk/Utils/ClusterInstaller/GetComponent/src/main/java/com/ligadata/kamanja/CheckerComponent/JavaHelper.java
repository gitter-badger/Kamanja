package com.ligadata.kamanja.CheckerComponent;

import java.io.StringWriter;
import org.apache.log4j.*;

public class JavaHelper {
    String component;
    String version;
    String nodeId;
    String status;
    static String errorMessage = null;
    StringWriter errors = new StringWriter();
    StringUtility strutl = new StringUtility();
    private Logger LOG = Logger.getLogger(getClass());

    private String CheckJavaVersion() {
        try {
            version = System.getProperty("java.version");
            status = "Success";
            return version;
        } catch (Exception e) {
            // e.printStackTrace(new PrintWriter(errors));
            // errorMessage = errors.toString();
            LOG.error("Failed to get Java information", e);
            status = "Fail";
            errorMessage = strutl.getStackTrace(e);
        }
        return null;
    }

    public void AskJava() {
        //JavaHelper java = new JavaHelper();
        //version = java.CheckJavaVersion();
        version = CheckJavaVersion();
        if (version == null || version.trim().length() == 0 || errorMessage != null)
            status = "Fail";
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getNodeId() {
        return nodeId;
    }

    public void setNodeId(String nodeId) {
        this.nodeId = nodeId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    @SuppressWarnings("static-access")
    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public StringWriter getErrors() {
        return errors;
    }

    public void setErrors(StringWriter errors) {
        this.errors = errors;
    }
}
