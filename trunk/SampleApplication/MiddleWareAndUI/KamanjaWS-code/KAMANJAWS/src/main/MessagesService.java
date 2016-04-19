/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package main;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.crypto.Data;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Seconds;
import org.joda.time.format.DateTimeFormat;


/**
 * Servlet implementation class MessagesService
 */
public class MessagesService extends HttpServlet {
	
	private static final long serialVersionUID = 1L;
	
	//private static EventsInfo PreviousEventsInfo = null;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public MessagesService() 
    {
        super();
        
    }

    public void init(ServletConfig config) throws ServletException {
		super.init(config);
		
	}
    
	
	public String getDataConnectionString()
	{
		return getServletConfig().getInitParameter("DataConnectionString");
	}
	public String getDBLoadToolCommand()
	{
		return getServletConfig().getInitParameter("DBLoadToolCommand");
	}
	public String getDBLoadToolProcessName()
	{
		return getServletConfig().getInitParameter("DBLoadToolProcessName");
	}
	
	public String getZookeeperConnnectionString()
	{
		return getServletConfig().getInitParameter("ZookeeperConnnectionString");
	}
	
	public String getLogFilePath()
	{
		return getServletConfig().getInitParameter("LogFilePath");
	}
	
	public String getLogFilePattern()
	{
		return getServletConfig().getInitParameter("LogFilePattern");
	}
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doPost(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException 
	{
		//response.setBufferSize(10024000);
		
		response.setContentType("text/xml");
		response.setContentType("text/html; charset=UTF-8");
		response.setCharacterEncoding("UTF-8");
		response.setHeader("Cache-Control", "no-cache");
		response.setHeader("Pragma", "no-cache");
		
		Global.CONFIG_LOG_FILE_PATH = getLogFilePath();
		Global.CONFIG_LOG_FILE_PATTERN = getLogFilePattern();
		
		HttpSession session = request.getSession(true);
		
		DateTime callTimestamp = DateTime.now();
		
		UserState userState = null;
		if (session != null)
		{
			
			if(session.getAttribute("npariostate") != null) 
			{
				userState = (UserState) session.getAttribute("npariostate");//dummy user state used for logging. no login is needed for now
			}
			else
			{
				userState = new UserState();
				session.setAttribute("npariostate", userState);
			}
		}
		
			
		//else 
		//{
			//throw new Exception("Session state can not be null");
		//}
		
		synchronized(session)
		{
		
		
			PrintWriter out = response.getWriter();
			
			if (request.getParameter("svc") == null)
			{
				response.sendError(400, "svc not specified");
	
			}
			else
			{
				switch (request.getParameter("svc").toLowerCase())
				{
					case "geteventsinfo":
						try
						{
							String reset = request.getParameter("reset");
							
								
							String result = getEventsInfo(request, reset!=null && reset.equalsIgnoreCase("true"), callTimestamp);
							out.print(result);
							
						}
						catch (Exception e)
						{
							e.printStackTrace();
							response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
						}
						break;
						
					case "getscrollalerts":
						try
						{
							String reset = request.getParameter("reset");
							
								
							String result = getScrollAlerts(request, reset!=null && reset.equalsIgnoreCase("true"));
							out.print(result);
							
						}
						catch (Exception e)
						{
							e.printStackTrace();
							response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
						}
						break;
						
					case "datareset":
						try
						{
								
							dataReset(request);
							out.print("");
							
						}
						catch (Exception e)
						{
							e.printStackTrace();
							response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
						}
						break;
						
					case "start":
						try
						{
							String start = request.getParameter("start");
							String result = startStopEngine(request, start);
							out.print(result);
							
						}
						catch (Exception e)
						{
							e.printStackTrace();
							response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
						}
						break;
						
					case "test":
						
						out.print("Testing.....");
						break;
						
					default:
						
						response.sendError(HttpServletResponse.SC_BAD_REQUEST);
						break;
				}
			}
			out.flush();
			out.close();
		}
	}
	
	
	public String dataReset(HttpServletRequest Httprequest)
	{
		/*try
		{//kill the tool if already running
			Utils.KillProcessByName(getDBLoadToolProcessName());
			
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}*/

		
		try
		{//run db load tool for demo
			
			//Utils.RunLoadTool(getDBLoadToolCommand());
			return(JsonHelper.wrapUpInXml("true"));
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			
			return(JsonHelper.wrapUpInXml("false"));
		}
		
		//return(JsonHelper.wrapUpInXml("true"));
	}
	
	
	public String startStopEngine(HttpServletRequest Httprequest, String start)
	{
		String methodName = Thread.currentThread().getStackTrace()[1].getMethodName();	
		try
		{
			
			Log.WriteMethodCall(methodName, "start: " + start + "", Httprequest);
			
			String zkConnection = getZookeeperConnnectionString();
			
			if(start.equals("1"))
			{
				MTServicesHelper.startEngine(Httprequest, zkConnection);
			}
			
			else if(start.equals("0"))
			{
				MTServicesHelper.stopEngine(Httprequest, zkConnection);
			}
			
			else
				throw new Exception("not supported argument value start='"+start+"'");
			
			String result = JsonHelper.wrapUpInXml("true");
			
			Log.WriteMethodResult(methodName, Httprequest);
			
			return result;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			Log.WriteMethodException(methodName, ex, Httprequest);
			return(JsonHelper.wrapUpInXml("false"));
		}
		
	}

	public String getEventsInfo(HttpServletRequest Httprequest, boolean reset, DateTime callTimestamp)
	{
		//Map<String, Object> result = getEventsInfoFromDB2(Httprequest, reset, callTimestamp);
		String methodName = Thread.currentThread().getStackTrace()[1].getMethodName();	
		Log.WriteMethodCall(methodName, "reset: " + reset + "", Httprequest);
		
		String zkConnection = getZookeeperConnnectionString();
		Map<String, Object> result = null;
		try
		{
			result = MTServicesHelper.getEventsInfoFromZookeeper(Httprequest, zkConnection, reset);
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
			
			Log.WriteMethodException(methodName, ex, Httprequest);
			return "";
		}
		
		Log.WriteMethodResult(methodName, Httprequest);
		
		if(result != null)
			return JsonHelper.SerializeList(result);
		
		return "";
	}
	
	private ClientEventsInfo getDumbEventsInfo()
	{
		int length = 7;
		
		ClientEventsInfo eventsInfo = new ClientEventsInfo();
		eventsInfo.TransactionsProcessed = RandomHelper.GenerateLong(length);
		eventsInfo.TotalAlertsQueued = RandomHelper.GenerateLong(length);
		eventsInfo.EndToEndLatency = RandomHelper.GenerateLong(length);
		eventsInfo.EngineLatencyForAlerts = RandomHelper.GenerateLong(length);
		
		eventsInfo.AlertsPerType = new LinkedHashMap<String, Long>();
		
		
		eventsInfo.AlertsPerType.put(AlertType.UTF.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.EB1.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.EB2.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.NOD.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.OD1.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.OD2.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.OD3.toString(), RandomHelper.GenerateLong(length));
		eventsInfo.AlertsPerType.put(AlertType.LB.toString(), RandomHelper.GenerateLong(length));
		
		return eventsInfo;
	}
	
	
	public Map<String, Object> getEventsInfoFromDB(HttpServletRequest request, boolean reset)
	{
		
		///////////////////////////////////////////////////////
		/*EventsInfo dumbEventsInfo =  new EventsInfo();
		dumbEventsInfo.Id = 1;
		dumbEventsInfo.NOD_Alerts = 1;
		
		
		dumbEventsInfo.TimeStamp = DateTime.parse("2014-07-15 22:53:37.010", DateTimeFormat.forPattern(Utils.TimeFormat));
    	
		dumbEventsInfo.TransactionsProcessed = 810;
		dumbEventsInfo.TotalAlerts = 90;
		dumbEventsInfo.UTF_Alerts = 0;
		dumbEventsInfo.EB1_Alerts = 2;
		dumbEventsInfo.EB2_Alerts = 1;
		dumbEventsInfo.NOD_Alerts = 4;
		dumbEventsInfo.OD1_Alerts = 4;
		dumbEventsInfo.OD2_Alerts = 14;
		dumbEventsInfo.OD3_Alerts = 22;
		dumbEventsInfo.LB_Alerts = 43;
		dumbEventsInfo.EndToEndLatency = 128641;
		dumbEventsInfo.EngineLatencyForAlerts = 99;
		
		PreviousEventsInfo = dumbEventsInfo;*/
    	////////////////////////////////////////////////////
		
		
		HttpSession session = request.getSession(true);
		EventsInfo PreviousEventsInfo = (EventsInfo)session.getAttribute("PreviousEventsInfo");
		AlertInfo LastAlert = (AlertInfo)session.getAttribute("LastAlert");
		long TimeOffsetMillis = 0;
		
		if(reset)
		{
			//dataReset(request);
			
			PreviousEventsInfo = null;
			LastAlert = null;
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
			session.setAttribute("LastAlert", LastAlert);
			
		}
		
		if(!reset)
			if(session.getAttribute("TimeOffsetMillis") != null)
				TimeOffsetMillis = (long)session.getAttribute("TimeOffsetMillis");
		
		ClientEventsInfo clientEventsInfo = new ClientEventsInfo();
		List<ClientAlertInfo> alertInfoList = new LinkedList<ClientAlertInfo>();
		
		EventsInfo eventsInfo = null;
		String pdQuery = "";
		/*if(PreviousEventsInfo == null && !reset)//we are done here
		{
			eventsInfo = null;
			clientEventsInfo = null;
		}*/
		
		//else
		{
			String condition = "";
			if(PreviousEventsInfo != null)
				condition = " where " + DataServices.PD_COLNAME_TimeStamp + ">'" + PreviousEventsInfo.TimeStamp.minus(TimeOffsetMillis).toString(Utils.TimeFormat) + "'";
			
			
			pdQuery = "select * from " + DataServices.PD_TBL_NAME + condition + " order by " + DataServices.PD_COLNAME_TimeStamp + " desc limit 1";
			
		}
		
		ResultSet rs;
		
		Connection dbcon = null;
		try
		{
			dbcon = DataServices.connectToDB(getDataConnectionString());
			
			if(!Utils.isStringNullOrEmpty(pdQuery))
			{
				//System.out.println(DateTime.now().toString(Utils.TimeFormat) + " running query: "+pdQuery);
				
				rs = DataServices.getResultSet(dbcon, pdQuery);
				if (rs.next())
		        {	            
					DateTime recordTimestamp = new DateTime(rs.getTimestamp(DataServices.PD_COLNAME_TimeStamp));
					
					if(reset) // first row
	            	{
	            		DateTime systemTimestamp = DateTime.now();//.withZone(DateTimeZone.UTC);
	            		TimeOffsetMillis = systemTimestamp.getMillis() - recordTimestamp.getMillis();
	            		System.out.println("setting TimeOffsetMillis="+TimeOffsetMillis);
	            		session.setAttribute("TimeOffsetMillis", TimeOffsetMillis);
	            	}
	            	
					eventsInfo =  new EventsInfo();
					eventsInfo.Id = rs.getLong(DataServices.PD_COLNAME_ID);
					eventsInfo.NodeId = rs.getInt(DataServices.PD_COLNAME_NodeId);
					eventsInfo.TimeStamp = recordTimestamp.plus(TimeOffsetMillis);
					
					eventsInfo.TransactionsProcessed = rs.getLong(DataServices.PD_COLNAME_EventsProcessed);
					eventsInfo.TotalAlerts = rs.getLong(DataServices.PD_COLNAME_Total_Alerts);
					eventsInfo.UTF_Alerts = rs.getLong(DataServices.PD_COLNAME_UTF_Alerts);
					eventsInfo.EB1_Alerts = rs.getLong(DataServices.PD_COLNAME_EB1_Alerts);
					eventsInfo.EB2_Alerts = rs.getLong(DataServices.PD_COLNAME_EB2_Alerts);
					eventsInfo.NOD_Alerts = rs.getLong(DataServices.PD_COLNAME_NOD_Alerts);
					eventsInfo.OD1_Alerts = rs.getLong(DataServices.PD_COLNAME_OD1_Alerts);
					eventsInfo.OD2_Alerts = rs.getLong(DataServices.PD_COLNAME_OD2_Alerts);
					eventsInfo.OD3_Alerts = rs.getLong(DataServices.PD_COLNAME_OD3_Alerts);
					eventsInfo.LB_Alerts = rs.getLong(DataServices.PD_COLNAME_LB_Alets);
					eventsInfo.EndToEndLatency = rs.getLong(DataServices.PD_COLNAME_EventsLatency);
	            	eventsInfo.EngineLatencyForAlerts = rs.getLong(DataServices.PD_COLNAME_AlertsLatency);
	            	
	            	
	            	
		        }
				else
				{
					eventsInfo = null;
					clientEventsInfo = null;
					
				}
				
		        
		        rs.close();
			}
	        
	        if(eventsInfo != null)
	        {
	        	clientEventsInfo = new ClientEventsInfo(eventsInfo);
	        	if(PreviousEventsInfo != null)
	        	{
		        	//int duration = Seconds.secondsBetween(PreviousEventsInfo.TimeStamp, eventsInfo.TimeStamp).getSeconds();
	        		double durationInSecsonds = (eventsInfo.TimeStamp.getMillis() - PreviousEventsInfo.TimeStamp.getMillis()) / 1000.0;
		        	
		        	if(durationInSecsonds > 0)
		        	{
		        		clientEventsInfo.TransactionsPerSecond = (long)((eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed) / durationInSecsonds);
			        	clientEventsInfo.AlertsPerSecond = (long)((eventsInfo.TotalAlerts - PreviousEventsInfo.TotalAlerts) / durationInSecsonds);
		        	}
		        	else
		        	{
		        		clientEventsInfo.TransactionsPerSecond = 0;
		        		clientEventsInfo.AlertsPerSecond = 0;
		        	}
		        	
		        	clientEventsInfo.EndToEndLatency = ((eventsInfo.EndToEndLatency - PreviousEventsInfo.EndToEndLatency) /
		        			(eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed)) / 1000.0;
	        	}
	        	else
	        	{
	        		clientEventsInfo.TransactionsPerSecond = 0;
	        		clientEventsInfo.AlertsPerSecond = 0;
	        	}
	        }
	        

	        //alerts
	        String alertCondition = "";
			if(LastAlert != null)
				alertCondition += " where " + DataServices.Alert_COLNAME_TimeStamp + ">'" + LastAlert.TimeStamp.minus(TimeOffsetMillis)+"'";
			
			String alertsQuery = "select * from " + DataServices.Alert_TBL_NAME + alertCondition;
			
			//System.out.println(DateTime.now().toString(Utils.TimeFormat) + " running alertsQuery: "+alertsQuery);
			
			rs = DataServices.getResultSet(dbcon, alertsQuery);
			if (rs.next())
	        {
				do
				{
					AlertInfo alertInfo = new AlertInfo();
					alertInfo.AlertType = rs.getString(DataServices.Alert_COLNAME_AlertType);
					alertInfo.MobileNumber = rs.getString(DataServices.Alert_COLNAME_MobileNumber);
					alertInfo.TimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_TimeStamp)).plus(TimeOffsetMillis);
					alertInfo.AlertText = "Hi";
					alertInfo.Balance = rs.getFloat(DataServices.Alert_COLNAME_RunLedgerAmount);
					alertInfo.MessagePostTimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_MsgPostTimeStamp)).plus(TimeOffsetMillis);
					alertInfo.AlertPostTimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_AlertPostTimeStamp)).plus(TimeOffsetMillis);
					
					alertInfo.AlertLatency = 
							(alertInfo.MessagePostTimeStamp.getMillis() - alertInfo.AlertPostTimeStamp.getMillis()) / 1000.0;
					
					alertInfoList.add(new ClientAlertInfo(alertInfo));
					
					LastAlert = alertInfo;
				}while(rs.next());
	        }
			rs.close();
			
			if(dbcon != null)
				DataServices.closeConnection(dbcon);
		}
		catch(Exception ex)
		{
			
		}
		
		
		Map<String, Object> result = new LinkedHashMap<String, Object> ();
		result.put("EventsInfo", clientEventsInfo);
		result.put("Alerts", alertInfoList);
		
		if(eventsInfo != null)
		{
			PreviousEventsInfo = eventsInfo;//caching
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
		}
		
		session.setAttribute("LastAlert", LastAlert);
		
		return result;
	}


	
	//this one does the scroll itself assuming data is already in db
	public Map<String, Object> getEventsInfoFromDB2(HttpServletRequest request, boolean reset, DateTime callTimestamp)
	{
		HttpSession session = request.getSession(true);
		
		System.out.println("getEventsInfoFromDB2 requested session id=" + request.getRequestedSessionId());
		System.out.println("sessionid="+session.getId());
		
		if(!session.getId().equalsIgnoreCase(request.getRequestedSessionId()))
			reset = true;
		
		
		EventsInfo PreviousEventsInfo = (EventsInfo)session.getAttribute("PreviousEventsInfo");
		AlertInfo LastAlert = (AlertInfo)session.getAttribute("LastAlert");
		
		long TimeOffsetMillis = 0;
		
		System.out.println("value from sesion=" + (session.getAttribute("TimeOffsetMillis2")==null? "null" : session.getAttribute("TimeOffsetMillis2").toString()));
		
		if(reset)
		{
			//dataReset(request);
			
			PreviousEventsInfo = null;
			LastAlert = null;
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
			session.setAttribute("LastAlert", LastAlert);
			
		}
		
		//if(!reset)
		if(session.getAttribute("TimeOffsetMillis2") != null)
				TimeOffsetMillis = (long)session.getAttribute("TimeOffsetMillis2");
		
		System.out.println("TimeOffsetMillis2="+TimeOffsetMillis);
		
		ClientEventsInfo clientEventsInfo = new ClientEventsInfo();
		List<ClientAlertInfo> alertInfoList = new LinkedList<ClientAlertInfo>();
		
		EventsInfo eventsInfo = null;
		String pdQuery = "";
		if(session.getAttribute("TimeOffsetMillis2")==null && !reset)
		{
			if(PreviousEventsInfo != null)
			{
				pdQuery = "select * from " + DataServices.PD_TBL_NAME 
						+ " where " + DataServices.PD_COLNAME_ID + ">" + PreviousEventsInfo.Id
						+ " order by " + DataServices.PD_COLNAME_TimeStamp + " asc limit 1";
			}
			
		}
		
		else
		{
			if(reset)
			{
				pdQuery = "select * from " + DataServices.PD_TBL_NAME + " order by " + DataServices.PD_COLNAME_TimeStamp + " asc limit 1";
			}
			else
			{
				String condition = " where ";
				if(PreviousEventsInfo != null)
					condition += DataServices.PD_COLNAME_TimeStamp + ">'" + PreviousEventsInfo.TimeStamp.minus(TimeOffsetMillis).toString(Utils.TimeFormat) + "' and ";
				
				condition += DataServices.PD_COLNAME_TimeStamp + "<='" + callTimestamp.minus(TimeOffsetMillis).toString(Utils.TimeFormat)+ "' ";
				
				pdQuery = "select * from " + DataServices.PD_TBL_NAME + condition + " order by " + DataServices.PD_COLNAME_TimeStamp + " desc limit 1";
			}
			
		}
		
		ResultSet rs;
		
		Connection dbcon = null;
		try
		{
			
			dbcon = DataServices.connectToDB(getDataConnectionString());
			
			if(!Utils.isStringNullOrEmpty(pdQuery))
			{
				System.out.println(DateTime.now().toString(Utils.TimeFormat) + " running pdQuery: "+pdQuery);
				
				rs = DataServices.getResultSet(dbcon, pdQuery);
				if (rs.next())
		        {	            
					DateTime recordTimestamp = new DateTime(rs.getTimestamp(DataServices.PD_COLNAME_TimeStamp));
					
					if(reset || TimeOffsetMillis==0) // first row
	            	{
	            		DateTime systemTimestamp = callTimestamp;//.withZone(DateTimeZone.UTC);
	            		TimeOffsetMillis = systemTimestamp.getMillis() - recordTimestamp.getMillis();
	            		System.out.println("setting TimeOffsetMillis="+TimeOffsetMillis);
	            		session.setAttribute("TimeOffsetMillis2", TimeOffsetMillis);
	            	}
	            	
					eventsInfo =  new EventsInfo();
					eventsInfo.Id = rs.getLong(DataServices.PD_COLNAME_ID);
					eventsInfo.NodeId = rs.getInt(DataServices.PD_COLNAME_NodeId);
					eventsInfo.TimeStamp = recordTimestamp.plus(TimeOffsetMillis);
					
					eventsInfo.TransactionsProcessed = rs.getLong(DataServices.PD_COLNAME_EventsProcessed);
					eventsInfo.TotalAlerts = rs.getLong(DataServices.PD_COLNAME_Total_Alerts);
					eventsInfo.UTF_Alerts = rs.getLong(DataServices.PD_COLNAME_UTF_Alerts);
					eventsInfo.EB1_Alerts = rs.getLong(DataServices.PD_COLNAME_EB1_Alerts);
					eventsInfo.EB2_Alerts = rs.getLong(DataServices.PD_COLNAME_EB2_Alerts);
					eventsInfo.NOD_Alerts = rs.getLong(DataServices.PD_COLNAME_NOD_Alerts);
					eventsInfo.OD1_Alerts = rs.getLong(DataServices.PD_COLNAME_OD1_Alerts);
					eventsInfo.OD2_Alerts = rs.getLong(DataServices.PD_COLNAME_OD2_Alerts);
					eventsInfo.OD3_Alerts = rs.getLong(DataServices.PD_COLNAME_OD3_Alerts);
					eventsInfo.LB_Alerts = rs.getLong(DataServices.PD_COLNAME_LB_Alets);
					eventsInfo.EndToEndLatency = rs.getLong(DataServices.PD_COLNAME_EventsLatency);
	            	eventsInfo.EngineLatencyForAlerts = rs.getLong(DataServices.PD_COLNAME_AlertsLatency);
	            	
	            	
	            	
		        }
				else
				{
					eventsInfo = null;
					clientEventsInfo = null;
					
				}
				
		        
		        rs.close();
			}
	        
	        if(eventsInfo != null)
	        {
	        	clientEventsInfo = new ClientEventsInfo(eventsInfo);
	        	if(PreviousEventsInfo != null)
	        	{
		        	//int duration = Seconds.secondsBetween(PreviousEventsInfo.TimeStamp, eventsInfo.TimeStamp).getSeconds();
	        		double durationInSecsonds = (eventsInfo.TimeStamp.getMillis() - PreviousEventsInfo.TimeStamp.getMillis()) / 1000.0;
		        	
		        	if(durationInSecsonds > 0)
		        	{
		        		clientEventsInfo.TransactionsPerSecond = (long)((eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed) / durationInSecsonds);
			        	clientEventsInfo.AlertsPerSecond = (long)((eventsInfo.TotalAlerts - PreviousEventsInfo.TotalAlerts) / durationInSecsonds);
		        	}
		        	else
		        	{
		        		clientEventsInfo.TransactionsPerSecond = 0;
		        		clientEventsInfo.AlertsPerSecond = 0;
		        	}
		        	
		        	clientEventsInfo.EndToEndLatency = ((eventsInfo.EndToEndLatency - PreviousEventsInfo.EndToEndLatency) /
		        			(eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed)) / 1000.0;
	        	}
	        	else
	        	{
	        		clientEventsInfo.TransactionsPerSecond = 0;
	        		clientEventsInfo.AlertsPerSecond = 0;
	        	}
	        }
	        

	        //alerts
	        
	        
	        /*String alertCondition = "";
	        DateTime nowTS = DateTime.now();
			if(LastAlert != null)
				alertCondition = " where " + DataServices.Alert_COLNAME_TimeStamp + ">'" + LastAlert.TimeStamp.minus(TimeOffsetMillis).toString(Utils.TimeFormat)+"' and ";
			else 
				alertCondition = " where ";
			
			alertCondition += DataServices.Alert_COLNAME_TimeStamp + "<='" + DateTime.now().minus(TimeOffsetMillis).toString(Utils.TimeFormat) + "'";;
			
			String alertsQuery = "select * from " + DataServices.Alert_TBL_NAME + alertCondition;
			
			System.out.println(DateTime.now().toString(Utils.TimeFormat) + " running alertsQuery: "+alertsQuery);
			
			rs = DataServices.getResultSet(dbcon, alertsQuery);
			if (rs.next())
	        {
				do
				{
					AlertInfo alertInfo = new AlertInfo();
					alertInfo.AlertType = rs.getString(DataServices.Alert_COLNAME_AlertType);
					alertInfo.MobileNumber = rs.getString(DataServices.Alert_COLNAME_MobileNumber);
					alertInfo.TimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_TimeStamp)).plus(TimeOffsetMillis);
					alertInfo.AlertText = "Hi";
					alertInfo.Balance = rs.getFloat(DataServices.Alert_COLNAME_RunLedgerAmount);
					alertInfo.MessagePostTimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_MsgPostTimeStamp)).plus(TimeOffsetMillis);
					alertInfo.AlertPostTimeStamp = new DateTime(rs.getTimestamp(DataServices.Alert_COLNAME_AlertPostTimeStamp)).plus(TimeOffsetMillis);
					
					alertInfo.AlertLatency = 
							(alertInfo.MessagePostTimeStamp.getMillis() - alertInfo.AlertPostTimeStamp.getMillis()) / 1000.0;
					
					alertInfoList.add(new ClientAlertInfo(alertInfo));
					
					LastAlert = alertInfo;
				}while(rs.next());
	        }
			rs.close();*/
			
			if(dbcon != null)
				DataServices.closeConnection(dbcon);
		}
		catch(Exception ex)
		{
			
		}
		
		
		Map<String, Object> result = new LinkedHashMap<String, Object> ();
		result.put("EventsInfo", clientEventsInfo);
		result.put("Alerts", alertInfoList);
		
		if(eventsInfo != null)
		{
			PreviousEventsInfo = eventsInfo;//caching
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
		}
		
		session.setAttribute("LastAlert", LastAlert);
		
		if(TimeOffsetMillis!=0)
			session.setAttribute("TimeOffsetMillis2", TimeOffsetMillis);
		
		
		System.out.println("leaving method getEventsInfoFromDB2 with session value="+session.getAttribute("TimeOffsetMillis2"));
		System.out.println("------------------------------------------------------------------");
		
		return result;
	}
	
	public  Map<String, Object> getEventsInfoFromDB3(HttpServletRequest request, boolean reset)
	{
		System.out.println("getEventsInfoFromDB3 requested session id=" + request.getRequestedSessionId());
		
		HttpSession session = request.getSession(true);
		System.out.println("sessionid="+session.getId());
		System.out.println("called with reset="+reset+"");
		System.out.println("value from sesion=" + (session.getAttribute("TimeOffsetMillis3")==null? "null" : session.getAttribute("TimeOffsetMillis3").toString()));
		
		if(session.getId().equalsIgnoreCase(request.getRequestedSessionId()))
			reset = true;
		
		if(reset)
			session.setAttribute("TimeOffsetMillis3", 45678);
		
		ClientEventsInfo clientEventsInfo = new ClientEventsInfo();
		List<ClientAlertInfo> alertInfoList = new LinkedList<ClientAlertInfo>();
		
		Map<String, Object> result = new LinkedHashMap<String, Object> ();
		result.put("EventsInfo", clientEventsInfo);
		result.put("Alerts", alertInfoList);
		
		System.out.println("leaving  with session value="+session.getAttribute("TimeOffsetMillis3"));
		System.out.println("------------------------------------------------------------------");
		
		return result;
	}
	
	public static int ScrollAlertsLimit = 50;
	public  String getScrollAlerts(HttpServletRequest request, boolean reset)
	{
		List<AlertDetailsInfo> result = new LinkedList<AlertDetailsInfo>();
		
		/*ResultSet rs;
		Connection dbcon = null;
		
		System.out.println("getScrollAlerts requested session id=" + request.getRequestedSessionId());
		
		HttpSession session = request.getSession(true);
		
		System.out.println("getScrollAlerts session id=" + session.getId());
		
		System.out.println("------------------------------------------------------------------");
		
		long LastRetrievedId = -1;
		long ScrollTimeOffsetMillis=0;
		
		if(reset)
		{
			session.setAttribute("LastRetrievedId", LastRetrievedId);
		}
		else
		{
			if(session.getAttribute("LastRetrievedId") != null)
				LastRetrievedId = (long)session.getAttribute("LastRetrievedId");
			if(session.getAttribute("ScrollTimeOffsetMillis") != null)
				ScrollTimeOffsetMillis = (long)session.getAttribute("ScrollTimeOffsetMillis");
		}
		
		String condition = LastRetrievedId > 0 ? " where " + DataServices.Alert_COLNAME_ID + " >" + LastRetrievedId + " " : "";
		String getAlertsQuery = "select * from " + DataServices.AlertDetails_TBL_NAME + condition + " order by " + DataServices.Alert_COLNAME_ID + " limit " + ScrollAlertsLimit;
		try
		{
			dbcon = DataServices.connectToDB(getDataConnectionString());
			
			//System.out.println(DateTime.now().toString(Utils.TimeFormat) + " running query: " + getAlertsQuery);
			boolean firstRecord = true;
			
			rs = DataServices.getResultSet(dbcon, getAlertsQuery);
			if (rs.next())
	        {	     
				do
				{
					DateTime msgPostTimestamp = new DateTime(rs.getTimestamp(DataServices.AlertDetails_COLNAME_MsgPostTS));
					DateTime alertPostTimestamp = new DateTime(rs.getTimestamp(DataServices.AlertDetails_COLNAME_AlertPostTS));
					
					if(reset && firstRecord) // first row
	            	{
	            		DateTime systemTimestamp = DateTime.now();//.withZone(DateTimeZone.UTC);
	            		ScrollTimeOffsetMillis = systemTimestamp.getMillis() - msgPostTimestamp.getMillis();
	            		session.setAttribute("ScrollTimeOffsetMillis", ScrollTimeOffsetMillis);
	            	}
	            	
					AlertDetailsInfo alert = new AlertDetailsInfo();
					alert.AccountId = rs.getString(DataServices.AlertDetails_COLNAME_ACCID);
					alert.CustomerId = rs.getString(DataServices.AlertDetails_COLNAME_CustID);
					alert.RunLedgerAmount = rs.getDouble(DataServices.AlertDetails_COLNAME_RunLedgerAmount);
					alert.Amount = rs.getDouble(DataServices.AlertDetails_COLNAME_Amount);
					alert.MessagePostTimeStamp = msgPostTimestamp.plus(ScrollTimeOffsetMillis).toString(Utils.TimeFormat);
					alert.AccountShortName = rs.getString(DataServices.AlertDetails_COLNAME_AccShortName);
					alert.MobileNumber = rs.getString(DataServices.AlertDetails_COLNAME_MobileNumber);
					
					alert.AlertLatency = (msgPostTimestamp.getMillis() -  alertPostTimestamp.getMillis()) / 1000.0;
					
					result.add(alert);
					
					LastRetrievedId = rs.getLong(DataServices.Alert_COLNAME_ID);
					
					firstRecord = false;
					
				}while (rs.next());
	        }	
		    rs.close();
			
			
			if(dbcon != null)
				DataServices.closeConnection(dbcon);
		}
		catch(Exception ex)
		{
			
		}
			
		session.setAttribute("LastRetrievedId", LastRetrievedId);
		*/
		return JsonHelper.SerializeList(result);
	}
	
	
	public static class EventsInfo
	{
		public long Id;
		public int NodeId;
		public DateTime TimeStamp;
		public long TransactionsProcessed;
		public long TotalAlerts;
		
		public long UTF_Alerts;
		public long EB1_Alerts;
		public long EB2_Alerts;
		public long NOD_Alerts;
		public long OD1_Alerts;
		public long OD2_Alerts;
		public long OD3_Alerts;
		public long LB_Alerts;
		
		public long EndToEndLatency;
		public long EngineLatencyForAlerts;
	}
	
	public static class ClientEventsInfo
	{
		public ClientEventsInfo()
		{
			super();
		}
		public ClientEventsInfo(EventsInfo eventsInfo)
		{
			this.TransactionsProcessed = eventsInfo.TransactionsProcessed;
			this.TotalAlertsQueued = eventsInfo.TotalAlerts;
			//this.EndToEndLatency = eventsInfo.EndToEndLatency;
			this.EngineLatencyForAlerts = eventsInfo.EngineLatencyForAlerts;
			
			this.AlertsPerType = new LinkedHashMap<String, Long>();
			
			this.AlertsPerType.put(AlertType.UTF.toString(), eventsInfo.UTF_Alerts);
			this.AlertsPerType.put(AlertType.EB1.toString(), eventsInfo.EB1_Alerts);
			this.AlertsPerType.put(AlertType.EB2.toString(), eventsInfo.EB2_Alerts);
			this.AlertsPerType.put(AlertType.NOD.toString(), eventsInfo.NOD_Alerts);
			this.AlertsPerType.put(AlertType.OD1.toString(), eventsInfo.OD1_Alerts);
			this.AlertsPerType.put(AlertType.OD2.toString(), eventsInfo.OD2_Alerts);
			this.AlertsPerType.put(AlertType.OD3.toString(), eventsInfo.OD3_Alerts);
			this.AlertsPerType.put(AlertType.LB.toString(), eventsInfo.LB_Alerts);
		}
		
		public long TransactionsProcessed;
		public long TotalAlertsQueued;
		public double EndToEndLatency;
		public long EngineLatencyForAlerts;
		public Map<String, Long> AlertsPerType;
		
		public long TransactionsPerSecond;
		public long AlertsPerSecond;
	}
	
	public static class AlertInfo
	{
		public String AlertType;
		public String MobileNumber;
		public String AlertText;
		public DateTime TimeStamp;
		public double Balance;
		
		public DateTime MessagePostTimeStamp;
		public DateTime AlertPostTimeStamp;
		public double AlertLatency;
	}
	
	public static class ClientAlertInfo
	{
		public ClientAlertInfo()
		{
			super();
		}
		public ClientAlertInfo(AlertInfo alertInfo)
		{
			this.AlertType = alertInfo.AlertType;
			this.MobileNumber = alertInfo.MobileNumber;
			this.AlertText = alertInfo.AlertText;
			this.Balance = alertInfo.Balance;
			this.TimeStamp = alertInfo.TimeStamp.toString(Utils.TimeFormat);
			this.AlertLatency = alertInfo.AlertLatency;
		}
		public String AlertType;
		public String MobileNumber;
		public String AlertText;
		public String TimeStamp;
		public double Balance;
		public double AlertLatency;
	}
	
	public static class AlertDetailsInfo
	{
		public String AccountId;
		public String CustomerId;
		public String MessagePostTimeStamp;
		public double AlertLatency;
		public double RunLedgerAmount;
		public double Amount;
		public String AccountShortName;
		public String MobileNumber;
	}
	
	public enum AlertType
	{
		UTF, EB1, EB2, NOD, OD1, OD2, OD3, LB
	}
}
