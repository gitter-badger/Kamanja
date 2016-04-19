/*package com.ligadata.samples.models;

import com.ligadata.KamanjaBase.*;
import com.ligadata.KamanjaBase.api.java.function.*;
import com.ligadata.samples.messages.*;
import com.google.common.base.Optional;

public class LowBalanceAlertModel2 extends ModelBase {

	static LowBalanceAlertModel2Obj objSignleton = new LowBalanceAlertModel2Obj();
	private TransactionContext txnContext = null;
    GlobalPreferences gPref = null;

    public ModelResult execute(boolean emitAllResults) {
        CustPreferences pref = null;
        JavaRDD<CustTransaction> rcntTxns = null;
        RddDate curDt = RddDate.currentDateTime();
        CustAlertHistory alertHistory = null;

        // First check the preferences and decide whether to continue or not
    	gPref = GlobalPreferences.getRecentOrNew();
        pref = CustPreferences.getRecentOrNew();

        if (pref.minBalanceAlertOptout() == false)
          return null;

        // Check if at least min number of hours elapsed since last alert
        alertHistory = CustAlertHistoryFactory.toJavaRDDObject().getRecentOrNew();

        if (curDt.timeDiffInHrs(new RddDate(alertHistory.alertDtTmInMs())) < gPref.minAlertDurationInHrs())
          return null;

        // get history of transaction whose balance is less than minAlertBalance in last N days
        TimeRange lookBackTime = curDt.lastNdays(gPref.numLookbackDaysForMultiDayMinBalanceAlert());
        rcntTxns = CustTransaction$.MODULE$.toJavaRDDObject().getRDD(lookBackTime, new MinBalanceDeterminator());

        // Getting Java RDD Object and performing operations on that
        if (!rcntTxns.isEmpty() || rcntTxns.count() < gPref.maxNumDaysAllowedWithMinBalance() ) {
          return null;
        }

        // compute distinct days in the retrieved history (number of days with balance less than minAlertBalance)
        // and check if those days meet the threshold

        int daysWhenBalanceIsLessThanMin = 0; // (rcntTxns.groupBy(new TransactionCorrelator())).count;

        if(daysWhenBalanceIsLessThanMin <= gPref.maxNumDaysAllowedWithMinBalance())
          return null;

        // create new alert history record and persist (if policy is to keep only one, this will replace existing one)
        CustAlertHistory.build().withAlertDtTmInMs(curDt.getDateTimeInMs()).withAlertType("lowBalanceAlert").Save();

        // ... Prepare results here ... need to populate result object with appropriate attributes
        return ModelResult.builder().withResult(new LowBalanceAlertResult2(txnContext)).build();
    }
    */

    /**
     * @param inTxnContext
     */
 /*   public LowBalanceAlertModel2 (ModelContext mdlContext) {
    	super(mdlContext, objSignleton);
    }

    // This is the object that needs to be implemented to
    class LowBalanceAlertResult2 {
    	private TransactionContext cxt;
    	public LowBalanceAlertResult2(TransactionContext inTxnContext) {
          cxt = inTxnContext;
    	}
    }

    // Implement this to use getRDD method on RDDObject
    class MinBalanceDeterminator implements Function1<CustTransaction, Boolean> {
      public Boolean call (CustTransaction ct) {
        //{ trn : CustTransaction => trn.balance < gPref.minAlertBalance }
        if (ct.balance() < gPref.minAlertBalance()) {
          return true;
        }
        return false;
      }
    }

    class TransactionCorrelator implements Function1<CustTransaction, Integer> {
      public Integer call(CustTransaction ct) {
        return new Integer(ct.date());
      }
    }

    public static class LowBalanceAlertModel2Obj implements ModelBaseObj {

    	public boolean IsValidMessage(ContainerInterface msg) {
    		return (msg instanceof CustTransaction);
    	}

    	public ModelBase CreateNewModel(ModelContext mdlContext) {
    		return new LowBalanceAlertModel2(mdlContext);
    	}

    	public String ModelName() {
    		return "LowBalanceAlertModel2Obj";
    	}

    	public String Version() {
    		return "0.0.1";
    	}
    }


}*/
