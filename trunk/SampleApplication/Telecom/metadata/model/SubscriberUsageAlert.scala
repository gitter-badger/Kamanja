// The following model generates an alert when an individual subscriber or account with sharedplan exceed usage
// above threshold as defined by a rate plan
// The following containers are used for lookup
// SubscriberGlobalPreferences : defines the thresholds in terms of percentage values
//                               at which we choose to call the message a warning or alert
// SubscriberInfo: individual subscriber information such as his phone number(msisdn), account number
//                 kind of ratePlan, activation date, preference to be notified or not
// SubscriberPlans: rate plan information such as kind of plan(shared or individual), plan limit, individual limit if any
// SubscriberAggregatedUsage: An object that maintains the aggregated usage for a given subscriber and for the current month
//                            Current Month is defined as whatever the month of the day, irrespective of activation date
//                            require enhancements if we aggregate the usage over each 30 days after activation
// AccountAggregatedUsage: An object that maintains the aggregated usage for a given account(could have
//                             multiple subscribers) and for the current month
//                            Current Month is defined as whatever the month of the day, irrespective of activation date
//                            require enhancements if we aggregate the usage over each 30 days after activation
// AccountInfo: individual account information such as account number, preference to be notified or not
//               Every subscriber is associated with an account
//     
package com.ligadata.models.samples.models

import com.ligadata.FatafatBase.{ BaseMsg, BaseContainer, RddUtils, RddDate, BaseContainerObj, MessageContainerBase, RDDObject, RDD }
import com.ligadata.FatafatBase.{ TimeRange, ModelBaseObj, ModelBase, ModelResultBase, TransactionContext, ModelContext }
import com.ligadata.messagescontainers.System.V1000000._
import RddUtils._
import RddDate._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import java.io.{ DataInputStream, DataOutputStream }
import org.apache.log4j.Logger
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime
import java.util.Locale

object SubscriberUsageAlert extends ModelBaseObj {
  override def IsValidMessage(msg: MessageContainerBase): Boolean = return msg.isInstanceOf[SubscriberUsage]
  override def CreateNewModel(mdlCtxt: ModelContext): ModelBase = return new SubscriberUsageAlert(mdlCtxt)
  override def ModelName(): String = "System.SubscriberUsageAlert" // Model Name
  override def Version(): String = "0.0.1" // Model Version
  override def CreateResultObject(): ModelResultBase = new SubscriberUsageAlertResult()
}

class SubscriberUsageAlertResult extends ModelResultBase {
  var msisdn: Long = 0;
  var curUsage: Long = 0
  var alertType: String = ""
  var triggerTime: Long = 0

  lazy val loggerName = this.getClass.getName
  lazy val logger = Logger.getLogger(loggerName)

  def withMsisdn(cId: Long): SubscriberUsageAlertResult = {
    msisdn = cId
    this
  }

  def withCurusage(curTotalUsage: Long): SubscriberUsageAlertResult = {
    curUsage = curTotalUsage
    this
  }

  def withAlertType(alertTyp: String): SubscriberUsageAlertResult = {
    alertType = alertTyp
    this
  }

  def withTriggerTime(triggerTm: Long): SubscriberUsageAlertResult = {
    triggerTime = triggerTm
    this
  }

  override def toJson: List[org.json4s.JsonAST.JObject] = {
    val json = List(
      ("Msisdn" -> msisdn) ~
        ("Curusage" -> curUsage) ~
        ("AlertType" -> alertType) ~
        ("TriggerTime" -> triggerTime))
    return json
  }

  override def toString: String = {
    compact(render(toJson))
  }

  override def get(key: String): Any = {
    if (key.compareToIgnoreCase("msisdn") == 0) return msisdn
    if (key.compareToIgnoreCase("curUsage") == 0) return curUsage
    if (key.compareToIgnoreCase("alertType") == 0) return alertType
    if (key.compareToIgnoreCase("triggerTime") == 0) return triggerTime
    return null
  }

  override def asKeyValuesMap: Map[String, Any] = {
    val map = scala.collection.mutable.Map[String, Any]()
    map("msisdn") = msisdn
    map("curusage") = curUsage
    map("alerttype") = alertType
    map("triggertime") = triggerTime
    map.toMap
  }

  override def Deserialize(dis: DataInputStream): Unit = {
    // BUGBUG:: Yet to implement
  }

  override def Serialize(dos: DataOutputStream): Unit = {
    // BUGBUG:: Yet to implement
  }
}


class AccountUsageAlertResult extends ModelResultBase {
  var actNo: Long = 0;
  var curUsage: Long = 0
  var alertType: String = ""
  var triggerTime: Long = 0

  lazy val loggerName = this.getClass.getName
  lazy val logger = Logger.getLogger(loggerName)

  def withAct(aId: Long): AccountUsageAlertResult = {
    actNo = aId
    this
  }

  def withCurusage(curTotalUsage: Long): AccountUsageAlertResult = {
    curUsage = curTotalUsage
    this
  }

  def withAlertType(alertTyp: String): AccountUsageAlertResult = {
    alertType = alertTyp
    this
  }

  def withTriggerTime(triggerTm: Long): AccountUsageAlertResult = {
    triggerTime = triggerTm
    this
  }

  override def toJson: List[org.json4s.JsonAST.JObject] = {
    val json = List(
      ("ActNo" -> actNo) ~
        ("Curusage" -> curUsage) ~
        ("AlertType" -> alertType) ~
        ("TriggerTime" -> triggerTime))
    return json
  }

  override def toString: String = {
    compact(render(toJson))
  }

  override def get(key: String): Any = {
    if (key.compareToIgnoreCase("actNo") == 0) return actNo
    if (key.compareToIgnoreCase("curUsage") == 0) return curUsage
    if (key.compareToIgnoreCase("alertType") == 0) return alertType
    if (key.compareToIgnoreCase("triggerTime") == 0) return triggerTime
    return null
  }

  override def asKeyValuesMap: Map[String, Any] = {
    val map = scala.collection.mutable.Map[String, Any]()
    map("actno") = actNo
    map("curusage") = curUsage
    map("alerttype") = alertType
    map("triggertime") = triggerTime
    map.toMap
  }

  override def Deserialize(dis: DataInputStream): Unit = {
    // BUGBUG:: Yet to implement
  }

  override def Serialize(dos: DataOutputStream): Unit = {
    // BUGBUG:: Yet to implement
  }
}

class SubscriberUsageAlert(mdlCtxt: ModelContext) extends ModelBase(mdlCtxt, SubscriberUsageAlert) {
  lazy val loggerName = this.getClass.getName
  lazy val logger = Logger.getLogger(loggerName)
  val df = DateTimeFormat.forPattern("yyyyMMdd").withLocale(Locale.US)
  
  private def getMonth(dt: String): Int = {
    val jdt = DateTime.parse(dt,df)
    jdt.monthOfYear().get()
  }

  private def getCurrentMonth : Int = {
    val jdt = new DateTime()
    jdt.monthOfYear().get()
  }

  override def execute(emitAllResults: Boolean): ModelResultBase = {
    var subMonthlyUsage:Long = 0
    var actMonthlyUsage:Long = 0

    // Get the current subscriber, account info and global preferences
    val gPref = SubscriberGlobalPreferences.getRecentOrNew(Array("Type1"))
    val subInfo = SubscriberInfo.getRecentOrNew
    val actInfo = AccountInfo.getRecentOrNew(Array(subInfo.actno))
    val planInfo  = SubscriberPlans.getRecentOrNew(Array(subInfo.planname))

    // Make sure current transaction has some data
    val rcntTxn = SubscriberUsage.getRecent
    if (rcntTxn.isEmpty) {
      return null
    }

    // Get current values of aggregatedUsage
    val subAggrUsage = SubscriberAggregatedUsage.getRecentOrNew
    val actAggrUsage = AccountAggregatedUsage.getRecentOrNew

    // Get current month
    val curDtTmInMs = RddDate.currentGmtDateTime
    val txnMonth = getMonth(rcntTxn.get.date.toString)
    val currentMonth = getCurrentMonth

    logger.info("this month usage => " + subAggrUsage.thismonthusage)
    logger.info("current usage  => " + rcntTxn.get.usage)
    logger.info("plan name => " + subInfo.planname)
    logger.info("plan type => " + planInfo.plantype)

    // if the usage doesn't belong to this month, we ignore it
    if( txnMonth != currentMonth ){
      logger.info("The month value is either old or incorrect,transaction ignored " + txnMonth)
      return null
    }

    // aggregate the usage 
    // aggregate individual subscriber usage
    subMonthlyUsage = subAggrUsage.thismonthusage + rcntTxn.get.usage
    SubscriberAggregatedUsage.build.withthismonthusage(subMonthlyUsage).Save
    // aggregate account usage
    actMonthlyUsage = actAggrUsage.thismonthusage + rcntTxn.get.usage
    AccountAggregatedUsage.build.withthismonthusage(actMonthlyUsage).Save
    logger.info("subscriber monthly usage => " + subMonthlyUsage + ",account monthly usage => " + actMonthlyUsage)

    // planLimit values are supplied as GB. But SubscriberUsage record contains the usage as MB
    // So convert planLimit to MB
    val planLimit = planInfo.planlimit * 1000
    val indLimit  = planInfo.individuallimit * 1000
    logger.info("plan limit => " + planLimit + ",individual limit => " + indLimit)

    val curTmInMs = curDtTmInMs.getDateTimeInMs
    
    // generate alerts if plan limits are exceeded based on planType
    planInfo.plantype match {
      case 1 => { // shared plans
	// exceeded plan limit
	if ( actMonthlyUsage > planLimit ){
	  if (actInfo.thresholdalertoptout == false) {
	    logger.info("Creating Model Result for account " + actInfo.actno)
	    return new AccountUsageAlertResult().withAct(actInfo.actno).withCurusage(actMonthlyUsage).withAlertType("pastThresholdAlert").withTriggerTime(curTmInMs)
	  }
	}
	// shared plan, but an individual limit may have been exceeded
	if ( subMonthlyUsage > indLimit ){
	  if (subInfo.thresholdalertoptout == false) {
	    logger.info("Creating Model Result for subscriber " + rcntTxn.get.msisdn)
	    return new SubscriberUsageAlertResult().withMsisdn(rcntTxn.get.msisdn).withCurusage(subMonthlyUsage).withAlertType("pastThresholdAlert").withTriggerTime(curTmInMs)
	  }
	}
      }
      case 2 => { // individual plans
	// individual plan,  individual limit may have been exceeded
	if ( subMonthlyUsage > indLimit ){
	  if (subInfo.thresholdalertoptout == false) {
	    logger.info("Creating Model Result for subscriber " + rcntTxn.get.msisdn)
	    return new SubscriberUsageAlertResult().withMsisdn(rcntTxn.get.msisdn).withCurusage(subMonthlyUsage).withAlertType("pastThresholdAlert").withTriggerTime(curTmInMs)
	  }
	}
      }
      case _ => {
	// unsupported plan type
	logger.info("Unknown planType => " + planInfo.plantype)
      }
    }
    return null
  }
}
