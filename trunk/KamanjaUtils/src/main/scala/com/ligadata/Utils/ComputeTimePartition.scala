
package com.ligadata.Utils;

import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat

object ComputeTimePartition {
  
   private def extractTime(fieldData: String, timeFormat: String): Long = {
    if (fieldData == null || fieldData.trim() == "") return 0

    if (timeFormat == null || timeFormat.trim() == "") return 0

    if (timeFormat.compareToIgnoreCase("epochtimeInMillis") == 0)
      return fieldData.toLong

    if (timeFormat.compareToIgnoreCase("epochtimeInSeconds") == 0 || timeFormat.compareToIgnoreCase("epochtime") == 0)
      return fieldData.toLong * 1000

    // Now assuming Date partition format exists.
    val dtFormat = new SimpleDateFormat(timeFormat);
    val tm =
      if (fieldData.size == 0) {
        new Date(0)
      } else {
        dtFormat.parse(fieldData)
      }
    tm.getTime()
  }

  def ComputeTimePartitionData(fieldData: String, timeFormat: String, timePartitionType: String): Long = {
    val fldTimeDataInMs = extractTime(fieldData, timeFormat)

    // Align to Partition
    var cal: Calendar = Calendar.getInstance();
    cal.setTime(new Date(fldTimeDataInMs));

    if (timePartitionType == null || timePartitionType.trim() == "") return 0

    timePartitionType.toLowerCase match {
      case "yearly" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(Calendar.YEAR, cal.get(Calendar.YEAR));
        return newcal.getTime().getTime()
      }
      case "monthly" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(Calendar.YEAR, cal.get(Calendar.YEAR))
        newcal.set(Calendar.MONTH, cal.get(Calendar.MONTH))
        return newcal.getTime().getTime()
      }
      case "daily" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
        return newcal.getTime().getTime()
      }
    }
    return 0
  }
}