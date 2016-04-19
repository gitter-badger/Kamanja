package com.ligadata.kamanja.CheckerComponent;

import java.io.StringWriter;

import com.ligadata.kamanja.CheckerComponent.StringUtility;
import org.apache.log4j.*;

public class ScalaHelper {
    String errorMessage = null;
    String version = null;
    String status;
    StringWriter errors = new StringWriter();
    StringUtility strutl = new StringUtility();
    private Logger LOG = Logger.getLogger(getClass());

    public StringWriter getErrors() {
        return errors;
    }

    public void setErrors(StringWriter errors) {
        this.errors = errors;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public String ScalaVersion() {
        try {
            StringUtility str = new StringUtility();
            StringBuffer output = new StringBuffer();
            String command = "which scala";
            String scalaLocation = "";
            String doc = "";

            try {
                /// root/Downloads/scala-2.10.4/bin/scala
                output = str.ExecuteSHCommandInputStream(command, 2000);
                scalaLocation = str.replaceSpacesFromString(output.toString());
                LOG.debug("Got: " + scalaLocation);
            } catch (Exception e) {
                LOG.error("Failed to get scala location using which scala", e);
            } catch (Throwable e) {
                LOG.error("Failed to get scala location using which scala", e);
            }


            // Try1
            if (!scalaLocation.isEmpty()) {
                StringBuffer output0 = new StringBuffer();
                command = scalaLocation + " -version 2>&1 | tee  /dev/null";

                try {
                    /// root/Downloads/scala-2.10.4/bin/scala
                    output0 = str.ExecuteSHCommandInputStream(command, 2000);
                    doc = output0.toString().trim().toLowerCase(); //str.replaceSpacesFromString(output0.toString().trim().toLowerCase());
                    LOG.debug("Got: " + doc);

                    if (doc.length() > 0) {
                        int beginIndex = str.IndexOfString(doc, "Scala code runner version".toLowerCase());
                        if (beginIndex >= 0) {
                            beginIndex += "Scala code runner version".length();
                        } else {
                            beginIndex = str.IndexOfString(doc, "2.");
                        }

                        if (beginIndex >= 0) {
                            int lastIndex = str.IndexOfStringFrom(doc, beginIndex, "--");
                            if (lastIndex > 0)
                                version = doc.substring(beginIndex, lastIndex).trim();
                            else
                                version = doc.substring(beginIndex).trim();
                            status = "Success";
                            return version;
                        }
                    }
                } catch (Exception e) {
                    LOG.error("Failed to get scala version using:" + command, e);
                } catch (Throwable e) {
                    LOG.error("Failed to get scala version using:" + command, e);
                }
            }

            // Try2
            try {
                StringBuffer output1 = new StringBuffer();
                command = "scala -version 2>&1 | tee  /dev/null";
                /// root/Downloads/scala-2.10.4/bin/scala
                output1 = str.ExecuteSHCommandInputStream(command, 2000);
                doc = output1.toString().trim().toLowerCase(); // str.replaceSpacesFromString(output1.toString().trim().toLowerCase());
                LOG.debug("Got: " + doc);

                if (doc.length() > 0) {
                    int beginIndex = str.IndexOfString(doc, "Scala code runner version".toLowerCase());
                    if (beginIndex >= 0) {
                        beginIndex += "Scala code runner version".length();
                    } else {
                        beginIndex = str.IndexOfString(doc, "2.");
                    }

                    if (beginIndex >= 0) {
                        int lastIndex = str.IndexOfStringFrom(doc, beginIndex, "--");
                        if (lastIndex > 0)
                            version = doc.substring(beginIndex, lastIndex).trim();
                        else
                            version = doc.substring(beginIndex).trim();
                        status = "Success";
                        return version;
                    }
                }
            } catch (Exception e) {
                LOG.error("Failed to get scala version using:" + command, e);
            } catch (Throwable e) {
                LOG.error("Failed to get scala version using:" + command, e);
            }

            // Try3
            try {
                doc = str.replaceSpacesFromString(output.toString().trim().toLowerCase());
                int beginIndex = str.IndexOfString(doc, "scala-");
                if (beginIndex >= 0) {
                    // System.out.println(beginIndex);
                    int lastIndex = str.IndexOfStringFrom(doc, beginIndex, "/");
                    // System.out.println(lastIndex);
                    // System.out.println(str.getWordBetweenIndex(doc, beginIndex +
                    // "scala-".length(), lastIndex));
                    if (lastIndex > 0)
                        version = doc.substring(beginIndex + "scala-".length(), lastIndex);
                    else
                        version = doc.substring(beginIndex + "scala-".length());
                    status = "Success";
                    return version;
                }
            } catch (Exception e) {
                LOG.error("Failed to get scala version from which scala", e);
            } catch (Throwable e) {
                LOG.error("Failed to get scala version using which scala", e);
            }

        } catch (Exception e) {
            // e.printStackTrace(new PrintWriter(errors));
            // errorMessage = errors.toString();
            LOG.error("Failed to get scala information", e);
            status = "Fail";
            errorMessage = strutl.getStackTrace(e);
        }
        return null;
    }

    // public void ScalaVersionsh() {
    // try {
    // //System.out.println(System.getProperty("user.name"));
    // StringBuffer output = new StringBuffer();
    // //String command = "source .bashrc";
    // StringUtility str = new StringUtility();
    // //output = str.ExecuteSHCommandErrStream(command);
    // String command = "scala -version";
    // // StringUtility str = new StringUtility();
    // output = str.ExecuteSHCommandErrStream(command);
    // // System.out.println(output.toString());
    // String doc =
    // str.replaceSpacesFromString(output.toString().trim().toLowerCase());
    // int beginIndex = str.IndexOfString(doc, "version");
    // int lastIndex = str.IndexOfString(doc, "copyright");
    // // System.out.println(beginIndex);
    // // System.out.println(lastIndex);
    // String version = str.getWordBetweenIndex(doc, beginIndex + 7, lastIndex -
    // 2);
    // System.out.println(version); // scala version
    // } catch (Exception e) {
    // // e.printStackTrace(new PrintWriter(errors));
    // // errorMessage = errors.toString();
    // errorMessage = strutl.getStackTrace(e);
    // }
    //
    // }

    @SuppressWarnings("unused")
    private void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public String getVersion() {
        return version;
    }

    @SuppressWarnings("unused")
    private void setVersion(String version) {
        this.version = version;
    }

    public String getStatus() {
        return status;
    }

    @SuppressWarnings("unused")
    private void setStatus(String status) {
        this.status = status;
    }

    public void AskScala() {
        version = ScalaVersion();
        if (version == null || version.trim().length() == 0 || errorMessage != null)
            status = "Fail";
    }

}
