current project is simple. it is an eclipse project. 

to build: in eclipse you can right-click file build.xml and choose run -> war file will be produced

project configuration is in file WebContent\WEB-INF\web.xml : 
	- parameter (ZookeeperConnnectionString)
	- for logging: LogFilePath - must be full path like /usr/share/tomcat6/KamanjaLogs/npario.log (if path is wrong app will still work, only log will not be written), of course the folder must give write permissions to the user running tomcat