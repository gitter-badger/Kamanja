package com.ligadata.kamanja.CheckerComponent;

import java.io.*;
import java.util.ArrayList;

import org.apache.log4j.BasicConfigurator;
import org.apache.zookeeper.KeeperException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.deser.std.ThrowableDeserializer;
import org.json.simple.JSONArray;
import org.apache.log4j.*;

public class Checker {
    private Logger LOG = Logger.getLogger(getClass());

    static StringWriter x = new StringWriter();

    StringUtility strutl = new StringUtility();

    private void getFailedComponentInfo(ComponentInfo bean, String component, Throwable e, String collectedErrorMessage) {
        bean.setVersion(null);
        bean.setStatus("Fail");

        if (collectedErrorMessage != null && e != null) {
            bean.setErrorMessage(collectedErrorMessage + strutl.getStackTrace(e));
        } else if (collectedErrorMessage != null) {
            bean.setErrorMessage(collectedErrorMessage);
        } else if (e != null) {
            bean.setErrorMessage(strutl.getStackTrace(e));
        }

        // bean.setInvocationNode(/* hostArray[i] */null);
        bean.setComponentName(component);
    }

    public /* ArrayList<ComponentInfo> */ComponentInfo CheckComponent(String component,
                                                                      String host/* String hostArray[] */) throws IOException, InterruptedException, KeeperException {

        // ArrayList<ComponentInfo> list = new ArrayList<ComponentInfo>();

        // for (int i = 0; i < hostArray.length; i++) {
        ComponentInfo bean = new ComponentInfo();
        switch (component.trim().toLowerCase()) {
            case "zookeeper":
                ZookeeperHelper zookeeper = new ZookeeperHelper();
                try {
                    zookeeper.askZookeeper(/* hostArray[i] */host);
                    bean.setVersion(zookeeper.getVersion());
                    bean.setStatus(zookeeper.getStatus());
                    bean.setErrorMessage(zookeeper.getErrorMessage());
                    // bean.setInvocationNode(/* hostArray[i] */null);
                    bean.setComponentName(component);
                } catch (Exception e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, zookeeper.getErrorMessage());
                } catch (Throwable e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, zookeeper.getErrorMessage());
                }
                break;
            case "kafka":
                KafkaHelper kafka = new KafkaHelper();
                try {
                    // System.out.println(hostArray[i]);
                    kafka.AskKafka(/* hostArray[i] */host);
                    // kafka.CheckKafkaVersion(hostArray[i]);
                    bean.setStatus(kafka.getStatus());
                    bean.setErrorMessage(kafka.getErrorMessage());
                    bean.setComponentName(component);
                    bean.setVersion(null);
                    // bean.setInvocationNode(null);
                } catch (Exception e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, kafka.getErrorMessage());
                } catch (Throwable e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, kafka.getErrorMessage());
                }
                break;
            case "java":
                JavaHelper java = new JavaHelper();
                try {
                    java.AskJava();
                    bean.setVersion(java.getVersion());
                    bean.setErrorMessage(java.getErrorMessage());
                    bean.setComponentName(component);
                    bean.setStatus(java.getStatus());
                } catch (Exception e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, java.getErrorMessage());
                } catch (Throwable e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, java.getErrorMessage());
                }
                break;
            case "scala":
                ScalaHelper scala = new ScalaHelper();
                try {
                    scala.AskScala();
                    bean.setVersion(scala.getVersion());
                    bean.setComponentName(component);
                    bean.setStatus(scala.getStatus());
                    bean.setErrorMessage(scala.getErrorMessage());
                } catch (Exception e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, scala.getErrorMessage());
                } catch (Throwable e) {
                    LOG.error("Failed to get " + component + " information", e);
                    getFailedComponentInfo(bean, component, e, scala.getErrorMessage());
                }
                break;
            default:
                LOG.error("Found Un-hanlded component:" + component);
                getFailedComponentInfo(bean, component, null, "Unhandled component");
                break;
        }

        // return list;
        return bean;
    }

    public String[] hostArray(String hostList) {
        return hostList.split(",");
    }

    public String CheckServices(String args) {
        try {
            if (args == null || args.trim().length() == 0) {
                throw new Exception();
            }
            JsonUtility json = new JsonUtility();
            Checker checker = new Checker();
            JSONArray jsonArray = new JSONArray();
            jsonArray = json.GetJsonArray(args);
            // ArrayList<ComponentInfo> list = null;
            ComponentInfo list = null;
            ArrayList<ComponentInfo> finalList = new ArrayList<ComponentInfo>();
            for (int i = 0; i < jsonArray.size(); i++) {
                json.JsonParse(jsonArray.get(i).toString());
                String component = json.GetComponent();
                String hostList = json.GetHostList();
                LOG.debug("Getting information for component:" + component);
                // String hostArray[] = checker.hostArray(hostList);
                if (component.equalsIgnoreCase("hbase")) {
                    try {
                        json.ParseOptionalField();
                        String authentication = json.getAuthentication();
                        String masterPrincipal = json.getMasterPrincipal();
                        String regionServer = json.getRegionServer();
                        String keyType = json.getKeyType();
                        String principal = json.getPrincipal();
                        list = checker.CheckHBaseComponent(component, /* hostArray */hostList, authentication,
                                masterPrincipal, regionServer, keyType, principal, json.getNamespace());
                        // System.out.println(authentication);
                        // System.out.println(masterPrincipal);
                        // System.out.println(regionServer);
                        // System.out.println(principal);
                        // System.out.println(keyType);
                    } catch (Exception e) {
                        LOG.error("Failed to get " + component + " information", e);
                        getFailedComponentInfo(new ComponentInfo(), component, e, null);
                    } catch (Throwable e) {
                        LOG.error("Failed to get " + component + " information", e);
                        getFailedComponentInfo(new ComponentInfo(), component, e, null);
                    }
                } else
                    list = checker.CheckComponent(component, /* hostArray */hostList);
                finalList.add(list);
            }
            return new ObjectMapper().writeValueAsString(finalList);
        } catch (Exception e) {
            LOG.error("Failed to get component info", e);
            System.out.println("Failed to get component info. Exception:" + strutl.getStackTrace(e));
        }
        return null;
    }

    private /* ArrayList<ComponentInfo> */ ComponentInfo CheckHBaseComponent(String component,
                                                                             String hostArray/* String[] hostArray */, String authentication, String masterPrincipal,
                                                                             String regionServer, String keyType, String principal, String namespace) throws IOException {
        // ArrayList<ComponentInfo> list = new ArrayList<ComponentInfo>();
        HBaseHelper hbase = new HBaseHelper();
        // for (int i = 0; i < hostArray.length; i++) {
        ComponentInfo bean = new ComponentInfo();

        try {
            hbase.AskHBase(/* hostArray[i] */hostArray, authentication, masterPrincipal, regionServer, keyType, principal, namespace);
            bean.setVersion(/*hbase.getVersion()*/null);
            bean.setStatus(hbase.getStatus());
            bean.setComponentName(component);
            bean.setErrorMessage(hbase.getErrorMessage());
        } catch (Exception e) {
            LOG.error("Failed to get hbase information", e);
            getFailedComponentInfo(new ComponentInfo(), component, e, hbase.getErrorMessage());
        } catch (Throwable e) {
            LOG.error("Failed to get hbase information", e);
            getFailedComponentInfo(new ComponentInfo(), component, e, hbase.getErrorMessage());
        }
        // bean.setInvocationNode(/* hostArray[i] */null);
        // }
        return bean;
    }

    private boolean WriteStringToFile(String flName, String str) {
        boolean writtenData = false;
        PrintWriter out = null;
        try {
            out = new PrintWriter(flName, "UTF-8");
            out.print(str);
            writtenData = true;
        } catch (Exception e) {
            LOG.error("Failed to write string into file:" + flName, e);
            System.out.println("Failed to open file:" + flName + "\nUsage: java -jar GetComponent-1.0 <OutputFile> <InputJsonArrayString>");
        } catch (Throwable t) {
            LOG.error("Failed to write string into file:" + flName, t);
            System.out.println("Failed to open file:" + flName + "\nUsage: java -jar GetComponent-1.0 <OutputFile> <InputJsonArrayString>");
        } finally {
            if (out != null)
                out.close();
        }
        return writtenData;
    }

    public static void main(String[] args) {
        /*
        // System.out.println(new Checker().CheckServices(
                // "[{\"component\":\"zookeeper\",\"hostlist\":\"localhost:2181,loclahost:2181\"},{\"component\":\"java\",\"hostlist\":\"192.168.10.20:2181,192.168.10.21:2181\"}]"
                // "[{\"component\":\"hbase\",\"hostlist\":\"localhost\",\"authentication\":\"kerberos\",\"regionserver_principal\":\"hbase/_HOST@INTRANET.LIGADATA.COM\",\"master_principal\":\"hbase/_HOST@INTRANET.LIGADATA.COM\",\"principal\":\"user@INTRANET.LIGADATA.COM\",\"keytab\":\"/apps/kamanja/CertificateInfo/user.keytab\"},{\"component\":\"java\",\"hostlist\":\"localhost\"},{\"component\":\"scala\",\"hostlist\":\"localhost\"},{\"component\":\"zookeeper\",\"hostlist\":\"localhost:2181\"},{\"component\":\"kafka\",\"hostlist\":\"localhost:9092\"}]"));
        */

        System.out.println("Got args count:" + args.length);
        for (String arg : args) {
            System.out.println("Arg:" + arg);
        }

        if (args.length != 2) {
            System.out.println("Usage: java -jar GetComponent-1.0 <OutputFile> <InputJsonArrayString>.");
            System.exit(1);
        }

        Checker checker = new Checker();
        try {
            if (checker.WriteStringToFile(args[0], "")) { // To make sure this file write is working
                String componentsInfo = checker.CheckServices(args[1]);
                if (componentsInfo != null)
                    checker.WriteStringToFile(args[0], componentsInfo);
            }
        } catch (Exception e) {
            if (checker != null && checker.LOG != null)
                checker.LOG.error("Failed to get component info", e);
            System.out.println("Failed to get component info. Exception:" + new StringUtility().getStackTrace(e));
            System.exit(1);
        } catch (Throwable e) {
            if (checker != null && checker.LOG != null)
                checker.LOG.error("Failed to get component info", e);
            System.out.println("Failed to get component info. Exception:" + new StringUtility().getStackTrace(e));
        }
    }
}
