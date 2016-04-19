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

import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.xml.soap.SOAPException;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;

import com.ligadata.biw.mtservices.MTServices;



public class UserState {

	protected static int MaxSessionInactiveTime = 20;

	public StringBuilder LogBuffer;
	
	public MTServices MTServicesObj = null;
	
	/*public String Email;

	public String FirstName;
	public boolean IsAdmin;
	public String LastName;
	
	public int LoginRetryCount;

	public String PasswordHash;
	public String Salt;
	public String ServerTime;

	public DateTime SessionStartTime;
	public DateTime LastActivityTime;
	public UserSessionStatus Status;
	public int UserId;

	public String Username;
	
	public String PlatformUserName;

	public int CompanyId;
	public String CompanyCode;

	public List<Integer> FavoriteSegments;

	public UserStateConfiguration UserConfiguration;// TODO: CHANGE TO
													// USERSTATECONFIGURATION
	
	public CustomUserStateConfiguration UserCustomConfiguration;
	
	public String CompanySchemaName;

	public int MaxRealSegmentsCount = 50;

	public int ActiveRealSegmentsCount;

	public boolean ChangePassword;

	public String SystemDimensions;

	public List<String> VisibleFeatures;

	public boolean FuseAuthentication;
	
	public String PdfFilePath;
	


	
	public boolean IsCompletelyRetrieved = false;*/
	
	public UserState() {
		//Email = "";
		//IsAdmin = false;
		//FavoriteSegments = new LinkedList<Integer>();
	}

	/*public static class UserStateUtils 
	{
		public static void Validate(String fnName, UserState usIn,
				HttpServletRequest request) throws Exception
		{

			HttpSession session = request.getSession(true);
			if (session.isNew()) {
				UserState us = new UserState();
				Utils.WriteToRequestLog("Session invalid, login please"
						+ request.getRemoteAddr(), us);
				throw new Exception("Session expired.");
			}

			if (session.getAttribute("npariostate") == null)
				throw new Exception("Session expired.");

			usIn = (UserState) session.getAttribute("npariostate");

			DateTime minimumAcceptableActivityTime  = DateTime.now().withZone(DateTimeZone.UTC).minusSeconds(session.getMaxInactiveInterval());
			
			if (usIn.LastActivityTime == null || usIn.LastActivityTime.isBefore(minimumAcceptableActivityTime))
	        {
	            //throw new TimeoutException();
	            throw new Exception("Session expired, last Activity:" + usIn.LastActivityTime.toString("yyyy/MM/dd HH:mm:ss"));
	        }
			
			if (usIn.Status != UserSessionStatus.Connected) {
				Utils.WriteToRequestLog("User '" + usIn.Email
						+ "' did not complete login" + request.getRemoteAddr(),
						usIn);

				throw new Exception("Invalid session", new Exception(
						"User did not complete login"));
			}

			// Check to make sure we got a valid emailaddress
			if (usIn == null || usIn.Email.isEmpty()) {
				Utils.WriteToRequestLog(
						"' User state is invalid as it does not contain emailaddress"
								+ request.getRemoteAddr(), usIn);

				throw new Exception(
						"Invalid session",
						new Exception(
								"User state is invalid as it does not contain emailaddress"));
			}
			
			if(!string.IsNullOrEmpty(fnName) && !fnName.equalsIgnoreCase("GetMigrationHistory") && !fnName.equalsIgnoreCase("GetSegmentsSize"))
			{//update last activity except for some methods that are auto-called by the client
				usIn.LastActivityTime = DateTime.now().withZone(DateTimeZone.UTC);
			}
			
			return;
		}
	}*/

}
