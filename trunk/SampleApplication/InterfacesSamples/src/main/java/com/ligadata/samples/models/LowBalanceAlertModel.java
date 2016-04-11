/*package com.ligadata.samples.models;

import com.ligadata.KamanjaBase.*;
import com.ligadata.samples.messages.*;
import com.google.common.base.Optional;


public class LowBalanceAlertModel extends ModelBase {

	static LowBalanceAlertModelObj objSignleton = new LowBalanceAlertModelObj();

    private TransactionContext txnContext = null;
    public ModelResult execute(boolean emitAllResults) {

        GlobalPreferences gPref = null;
        CustPreferences pref = null;
        com.google.common.base.Optional<CustTransaction> rcntTxnOp = com.google.common.base.Optional.absent();
        RddDate curDt = RddDate.currentDateTime();
        CustAlertHistory alertHistory = null;

        // First check the preferences and decide whether to continue or not
    	gPref = GlobalPreferences$.MODULE$.toJavaRDDObject().getRecentOrNew();
        pref = CustPreferences$.MODULE$.toJavaRDDObject().getRecentOrNew();

        if (pref.minBalanceAlertOptout() == false)
          return null;

        // Check if at least min number of hours elapsed since last alert
        alertHistory = CustAlertHistoryFactory.toJavaRDDObject().getRecentOrNew();

        if (curDt.timeDiffInHrs(new RddDate(alertHistory.alertDtTmInMs())) < gPref.minAlertDurationInHrs())
          return null;

        // Getting Java RDD Object and performing operations on that
        rcntTxnOp = CustTransaction$.MODULE$.toJavaRDDObject().getRecent();
        if (!rcntTxnOp.isPresent() || rcntTxnOp.get().balance() > gPref.minAlertBalance()) {
          return null;
        }

        // create new alert history record and persist (if policy is to keep only one, this will replace existing one)
        CustAlertHistory.build().withAlertDtTmInMs(curDt.getDateTimeInMs()).withAlertType("lowBalanceAlert").Save();

        // ... Prepare results here ... need to populate result object with appropriate attributes
        return ModelResult.builder().withResult(new LowBalanceAlertResult(txnContext)).build();
    }
	*/
/**
 * @param inTxnContext
 */ /*
    public LowBalanceAlertModel (ModelContext mdlContext) {
    	super(mdlContext, objSignleton);
    }


    // This is the object that needs to be implemented to
    class LowBalanceAlertResult {
    	private TransactionContext cxt;
    	public LowBalanceAlertResult(TransactionContext inTxnContext) {
          cxt = inTxnContext;
    	}
    }

    public static class LowBalanceAlertModelObj implements ModelBaseObj {

    	public boolean IsValidMessage(ContainerInterface msg) {
    		return (msg instanceof CustTransaction);
    	}

    	public ModelBase CreateNewModel(ModelContext mdlContext) {
    		return new LowBalanceAlertModel(mdlContext);
    	}

    	public String ModelName() {
    		return "JavaTestMdl";
    	}

    	public String Version() {
    		return "0.0.1";
    	}

    }

}*/


