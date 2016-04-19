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

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import main.MessagesService.ClientAlertInfo;
import main.MessagesService.ClientEventsInfo;
import main.MessagesService.EventsInfo;

import com.ligadata.biw.mtservices.*;
import com.ligadata.biw.mtservices.bean.Aggregations;

public class MTServicesHelper 
{
	public static void test()
	{
		/*try
		{
			String zkConnection = "192.168.200.140:2181";
			MTServices mt = new MTServices(zkConnection);
			System.out.println("all events = "+mt.getEventsProcessed());
			System.out.println("total alerts = "+mt.getTotalAlerts());
			
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}*/
	}

	public static MTServices getMtServicesInstance(HttpServletRequest request, String zkConnection)
	{
		UserState userState = (UserState) request.getSession().getAttribute("npariostate");
		
		if(userState.MTServicesObj == null)
		{
			System.out.println("Trying to create a new instance of MtServices with zookeeper connection: " + zkConnection);
			
			try
			{
				userState.MTServicesObj = new MTServices(zkConnection);
				System.out.println("A new instance of MtServices was successfully created");
			}
			catch(Exception ex)
			{
				System.out.println("Failed to create a new instance of MtServices");
				ex.printStackTrace();
			}
		}
		
		return userState.MTServicesObj;
	}
	
	public static void startEngine(HttpServletRequest request, String zkConnection) throws Exception
	{
		
		MTServices mt = getMtServicesInstance(request, zkConnection);
		
		if(mt != null)
			mt.start();
	}
	
	public static void stopEngine(HttpServletRequest request, String zkConnection) throws Exception
	{
		MTServices mt = getMtServicesInstance(request, zkConnection);
		
		if(mt != null)
			mt.stop();
	}
	
	public static Map<String, Object> getEventsInfoFromZookeeper(HttpServletRequest request, String zkConnection, boolean reset) throws Exception
	{
		Map<String, Object> result = new LinkedHashMap<String, Object>();
		//return result;
		
		HttpSession session = request.getSession(true);
		
		MTServices mt = getMtServicesInstance(request, zkConnection);
		
		if(mt == null)
			return result;
		
		EventsInfo PreviousEventsInfo = (EventsInfo)session.getAttribute("PreviousEventsInfo");
		
		
		if(reset)
		{
			//mt.start();
			
			PreviousEventsInfo = null;
			//LastAlert = null;
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
			//session.setAttribute("LastAlert", LastAlert);
			
		}
		
		
		
		ClientEventsInfo clientEventsInfo = new ClientEventsInfo();
		List<ClientAlertInfo> alertInfoList = new LinkedList<ClientAlertInfo>();
		
		EventsInfo eventsInfo =  new EventsInfo();
		//eventsInfo.Id = rs.getLong(DataServices.PD_COLNAME_ID);
		//eventsInfo.NodeId = rs.getInt(DataServices.PD_COLNAME_NodeId);
		
		
		Aggregations aggregations = mt.getAggregations();
		
		//eventsInfo.TimeStamp = new DateTime( mt.getMsgTimestamp()); //need to see format
		eventsInfo.TransactionsProcessed = mt.getEventsProcessed(); //???????????????
		eventsInfo.TotalAlerts = aggregations.getTotalAlerts();//mt.getTotalAlerts();
		eventsInfo.UTF_Alerts = aggregations.getUtfAlerts();//mt.getUTFAlerts();
		eventsInfo.EB1_Alerts = aggregations.getEb1Alerts();//mt.getEB1Alerts();
		eventsInfo.EB2_Alerts = aggregations.getEb2Alerts();//mt.getEB2Alerts();
		eventsInfo.NOD_Alerts = aggregations.getNodAlerts();//mt.getNODAlerts();
		eventsInfo.OD1_Alerts = aggregations.getOd1Alerts();// mt.getOD1Alerts();
		eventsInfo.OD2_Alerts = aggregations.getOd2Alerts();// mt.getOD2Alerts();
		eventsInfo.OD3_Alerts = aggregations.getOd3Alerts();// mt.getOD3Alerts();
		eventsInfo.LB_Alerts =  aggregations.getLbAlerts();// mt.getLBAlerts();
		//eventsInfo.EndToEndLatency = mt.get
    	//eventsInfo.EngineLatencyForAlerts = rs.getLong(DataServices.PD_COLNAME_AlertsLatency);
		
		
		clientEventsInfo = new ClientEventsInfo(eventsInfo);
    	if(PreviousEventsInfo != null)
    	{
        	//int duration = Seconds.secondsBetween(PreviousEventsInfo.TimeStamp, eventsInfo.TimeStamp).getSeconds();
    		double durationInSecsonds = 0;
    		if(eventsInfo.TimeStamp != null)
    			durationInSecsonds = (eventsInfo.TimeStamp.getMillis() - PreviousEventsInfo.TimeStamp.getMillis()) / 1000.0;
        	
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
        	
        	if((eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed) != 0)
	        	clientEventsInfo.EndToEndLatency = ((eventsInfo.EndToEndLatency - PreviousEventsInfo.EndToEndLatency) /
	        			(eventsInfo.TransactionsProcessed - PreviousEventsInfo.TransactionsProcessed)) / 1000.0;//TODO : end to end latency required
        	else 
        		clientEventsInfo.EndToEndLatency= 0 ;
    	}
    	else
    	{
    		clientEventsInfo.TransactionsPerSecond = 0;
    		clientEventsInfo.AlertsPerSecond = 0;
    	}
		
    	if(eventsInfo != null)
		{
			PreviousEventsInfo = eventsInfo;//caching
			session.setAttribute("PreviousEventsInfo", PreviousEventsInfo);
		}
    	
    	
    	List<String> messagesList = new LinkedList<String>();
    	String messeagesStr = aggregations.getLatestMsgs();
    	if(messeagesStr != null && messeagesStr.length()>0)
    	{
    		String[] msgsAr = messeagesStr.split(",");
    		messagesList = Arrays.asList(msgsAr);
    	}
    	
		result.put("EventsInfo", clientEventsInfo);
		result.put("Alerts", alertInfoList);
		result.put("MessageScrollAlerts", messagesList);
	
		return result;
	}
}
