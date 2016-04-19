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
import java.sql.Connection;
import java.sql.Date;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.joda.time.DateTime;

//import java.util.LinkedHashMap;
//import org.joda.time.DateTime;



public class DataServices
{
	private static final String DRIVER_CLASS_NAME = "org.postgresql.Driver";



	public static DBConnectionData parseConnectionString(String fullConnectionString) throws Exception {
		String connectionString = "", userName = "", password = "";
		Map<String, String> connectionData = new HashMap<String, String>();
		List<String> dataList = Arrays.asList(fullConnectionString.split("\\s*;\\s*"));
		for (String s : dataList) {
			String[] pair = s.split("\\s*=\\s*");
			connectionData.put(pair[0].trim(), pair.length <= 1 ? "" : pair[1].trim());
		}
		if (!connectionData.containsKey("Server") || !connectionData.containsKey("Port") || !connectionData.containsKey("Database")
				|| !connectionData.containsKey("User Id") || !connectionData.containsKey("Password"))
			throw new Exception("Error parsing connection string: " + fullConnectionString);
		connectionString = "jdbc:postgresql://" + connectionData.get("Server") + ":" + connectionData.get("Port") + "/"
				+ connectionData.get("Database");
		userName = connectionData.get("User Id");
		password = connectionData.get("Password");

		int port = Integer.parseInt(connectionData.get("Port"));
		return new DBConnectionData(connectionString, userName, password, connectionData.get("Server"), port,
				connectionData.get("Database"));
	}



	public static Connection connectToDB(String connectionString) {
		Connection result = null;
		try {
			Class.forName(DRIVER_CLASS_NAME).newInstance();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		try
		{
			//----------------------just for testing---------------------
			/*String server = "ls20.dc.npario.com";
			String database = "ls20";
			String userName = "admin";
			String password = "";
			int port = 5432;
			String connectionString = "jdbc:postgresql://" + server + ":" + port + "/"+ database;


			Global.CONNECTION_DATA = new DBConnectionData(connectionString, userName, password, server, port, database);*/

			//-------------------------------------------

			Global.CONNECTION_DATA = DataServices.parseConnectionString(connectionString);
			result = DriverManager.getConnection(Global.CONNECTION_DATA.connectionString, Global.CONNECTION_DATA.userName, Global.CONNECTION_DATA.password);

		}
		catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}


	public static void closeConnection(Connection connection)
	{
		try
		{
			if(connection != null )
				connection.close();
		}
		catch(SQLException ex)
		{

		}
	}

	public static ResultSet getResultSet(Connection conn, String select_string) {
		ResultSet rs = null;

		try
		{
			PreparedStatement s = conn.prepareStatement(select_string);
			rs = s.executeQuery();
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return rs;
	}

	public static void executeNonQuery(final Connection conn, final String query) throws SQLException {
		PreparedStatement s = null;
		try
		{
			s = conn.prepareStatement(query);
			s.execute();
			s.close();
			// conn.close();
		} catch (Exception e) {
			throw e;
		}
	}


	public static String readScalarString(final Connection conn, final String query) throws SQLException {
		ResultSet rs = getResultSet(conn, query);
		if (rs.next()) {
			String strValue = rs.getString(1);
			if (strValue != null) {
				return strValue;
			}
		}

		return "";
	}

	public static int readScalarInteger32(final Connection conn, final String select_str) {
		ResultSet rs = getResultSet(conn, select_str);
		try {
			if (rs.next())
				return rs.getInt(1);
		} catch (SQLException e) {
			// DO NOTHING
		}
		try {
			rs.close();
		} catch (SQLException e) {
		}
		return -1;
	}


	public static long readScalarInteger64(final Connection conn, final String select_str) throws SQLException {
		try {
			ResultSet rs = getResultSet(conn, select_str);
			if (rs.next()) {
				rs.getLong(1);
				if (rs.wasNull()) {
					rs.close();
					return 0;
				}
				return rs.getLong(1);
			}
			if (rs != null && !rs.isClosed())
				rs.close();
		} catch (SQLException e) {
		}
		return -1;
	}

	public static DateTime GetDateTimeFromResultSet(ResultSet rs, String colName) throws SQLException
	{
		if (rs.getObject(colName) == null)
			return Utils.dtNull;
		Date date = rs.getDate(colName);

		String strYear = formatYear.format(date);
		String strMonth = formatMonth.format(date);
		String strDay = formatDay.format(date);

		int year = Integer.parseInt(strYear);
		int month = Integer.parseInt(strMonth);
		int day = Integer.parseInt(strDay);
		DateTime datetime = new DateTime(year, month, day, 0, 0, 0);

		return datetime;
	}

	public static final SimpleDateFormat formatYear = new SimpleDateFormat("yyyy");
	public static final SimpleDateFormat formatMonth = new SimpleDateFormat("MM");
	public static final SimpleDateFormat formatDay = new SimpleDateFormat("dd");


	public static class DBConnectionData {
		final public String connectionString;
		final public String userName;
		final public String password;

		final String host;
		final int port;
		final String databaseName;

		public DBConnectionData(String connectionString, String userName,
				String password, String host, int port, String db) {
			this.connectionString = connectionString;
			this.userName = userName;
			this.password = password;
			this.host = host;
			this.port = port;
			this.databaseName = db;
		}
	}


	public static final String Schema_Name = "bar_kamanja";

	public static final String PD_TBL_NAME = Schema_Name + ".PD";

    public static final String PD_COLNAME_ID = "id";
    public static final String PD_COLNAME_NodeId = "nodeid";
    public static final String PD_COLNAME_TimeStamp = "message_timestamp";
    public static final String PD_COLNAME_EventsProcessed = "events_processed";
    public static final String PD_COLNAME_Total_Alerts = "total_alerts";
    public static final String PD_COLNAME_UTF_Alerts = "utf_alerts";
    public static final String PD_COLNAME_EB1_Alerts = "eb1_alerts";
    public static final String PD_COLNAME_EB2_Alerts = "eb2_alerts";
    public static final String PD_COLNAME_NOD_Alerts = "nod_alerts";
    public static final String PD_COLNAME_OD1_Alerts = "od1_alerts";
    public static final String PD_COLNAME_OD2_Alerts = "od2_alerts";
    public static final String PD_COLNAME_OD3_Alerts = "od3_alerts";
    public static final String PD_COLNAME_LB_Alets = "lb_alets";
    public static final String PD_COLNAME_EventsLatency = "events_latency";
    public static final String PD_COLNAME_AlertsLatency = "alerts_latency";


    public static final String Alert_TBL_NAME = Schema_Name + ".alert";

    public static final String Alert_COLNAME_ID = "id";
    public static final String Alert_COLNAME_NodeId = "nodeid";
    public static final String Alert_COLNAME_TimeStamp = "alert_timestamp";
    public static final String Alert_COLNAME_AlertType = "alert_type";
    public static final String Alert_COLNAME_MobileNumber = "mobile_number";
    public static final String Alert_COLNAME_Amount = "amount";
    public static final String Alert_COLNAME_RunLedgerAmount = "run_ledger_amount";
    public static final String Alert_COLNAME_MsgPostTimeStamp = "message_post_timestamp";
    public static final String Alert_COLNAME_AlertPostTimeStamp = "alert_post_timestamp";



    public static final String AlertDetails_TBL_NAME = Schema_Name + ".alert_details";
    public static final String AlertDetails_COLNAME_ID = "id";
    //public static final String AlertDetails_COLNAME_ID = "nodeid";
    public static final String AlertDetails_COLNAME_ACCID = "account_id";
    public static final String AlertDetails_COLNAME_CustID = "customer_id";
    public static final String AlertDetails_COLNAME_AlertType = "alert_type";
    public static final String AlertDetails_COLNAME_MsgPostTS = "message_post_timestamp";
    public static final String AlertDetails_COLNAME_AlertPostTS = "alert_post_timestamp";
    public static final String AlertDetails_COLNAME_RunLedgerAmount = "run_ledger_amount";
    public static final String AlertDetails_COLNAME_Amount = "amount";
    public static final String AlertDetails_COLNAME_AccShortName = "acct_short_name";
    public static final String AlertDetails_COLNAME_MobileNumber = "mobile_number";



}