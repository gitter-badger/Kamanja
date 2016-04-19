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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.http.HttpServletRequest;

import org.joda.time.DateTime;

public class Utils 
{
	public static boolean isStringNullOrEmpty(String s)
	{
		return (s==null || s.length()==0);
	}
	
	public final static DateTime dtNull = new DateTime(2999, 12, 31, 0, 0, 0);
	public static boolean isDtNull(DateTime date)
	{
		return date.equals(dtNull);
	}
	
	public static String TimeFormat = "yyyy-MM-dd HH:mm:ss.SSS";
	
	
	
	
	public static void RunLoadTool(String command) {

		String argumentsStr = "";
		
		RunProcess(command, argumentsStr);

	}

	public static String GetCurrentWebMethodName(HttpServletRequest request)
	{//there is no actual web method. assuming all urls are like <Servlet>?svc=<method>&..... => get the <method>
		if (request.getParameter("svc") == null) 
		{
			return "";
		} 
		else
		{
			return request.getParameter("svc");
		}
	}
	public static final void RunProcess(
			String command,
			String statupParameters)
	{
		ProcessBuilder pb = new ProcessBuilder(command, statupParameters);
		Process p = null;

		try
		{
			p = pb.start();
			
//			readOutput(p, request);
//			readError(p, request);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public static StringBuffer executeCommand(String command)
    {
		StringBuffer sb = new StringBuffer();
		try
		{
			Process p = Runtime.getRuntime().exec(command);
			p.waitFor();
			Thread.sleep(1000);
		    
		 
		    BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
		 
		    String line = "";
		    
		    while ((line = reader.readLine())!= null) 
		    {
		    	sb.append(line + "\n");
		    }
		    
		    System.out.println("Successfully running command " + command + " with result:" + sb.toString());
		    
		    return sb;
		}
		catch(Exception ex)
		{
			ex.printStackTrace();
		}
		return null;
    }
	
	public static final String executeCommandWaitResult(String[] command)
	{
		Process proc = null;

		try {

			proc = Runtime.getRuntime().exec(command);
			//MonitoringTool.LOG.info("Started with no errors " + proc.toString());
			
			BufferedReader reader = new BufferedReader(new InputStreamReader(proc.getInputStream()));
			
			StringBuffer sb = new StringBuffer();
			
			String line = null;
			while ((line = reader.readLine()) != null)
				sb.append(line + "\n");
			
			return sb.toString();
		}
		catch (Exception e)
		{
			//MonitoringTool.LOG.error("Error while running a command");
			return "";
		}
	}
	
	public static void KillProcessByName(String process){
		
		String command = String.format("pkill -f '%s'", process);
		
		executeCommand(command);
	}
}
