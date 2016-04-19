Notes on Kaggle "Carvana - Don't Get Kicked!""


Here are the assumptions and approach that I would take based upon our conversation when you were catching train. Please feel free to annotate and return to me, guiding me away from any misconceptions.

************

By using zip, you are suggesting that, at least for the first filter, it is sufficient to look at auction sites located in these zip code ranges.

Assumption Rule number 7's tuples are an array of ranges:

That is,


Rule number: 7 [IsBadBuy=1 cover=1717 (3%) prob=0.86]
 WheelTypeID=NULL
 TFC_VNZIP1=(3106,8505],(17028,17406],(19440,20166],(21014,21075],(21075,22403],(22403,22801],(25071,25177],(25177,26431],(26431,27407],(27542,28273],(28273,28625],(28625,29070],(29323,29461],(29532,29697],(29697,30120],(30120,30212],(32124,32219],(32503,32750],(33073,33311],(33411,33619],(33809,33916],(33916,34203],(34761,35004],(35004,35613],(37122,37138],(37421,37771],(37771,38118],(38128,38637],(42104,43207],(45005,45011],(45011,46239],(46803,47129],(48265,50111],(60443,60445],(64153,64161],(70002,70401],(70460,71119],(71119,72117],(72117,73108],(73129,74135],(74135,75020],(75050,75061],(76040,76063],(76101,77041],(77041,77061],(77061,77073],(77073,77086],(78219,78227],(78426,78610],(78745,78754],(78754,79605],(79932,80011],(80011,80022],(80112,80229],(80229,80817],(83687,83716],(84087,84104],(84104,85009],(85018,85040],(85204,85226],(85248,85260],(85260,85284],(85338,85353],(85353,87105],(89506,90045],(90650,91752],(91752,91763],(91770,92057],(92101,92337],(92337,92504],(92504,92807],(94544,95673]



Its simple representation : Array[IntRange] ... that is an array of tuples ... its type name might be ArrayOfIntRange
where IntRange = Tuple2[Int]

The data for this would be loaded into the array from kv store made available through the EnvContext singleton made available to each model by the Kamanja engine.  For example, tables can be loaded with something like this directly into the model:


<DerivedField name="SuspectAuctionZipRangeBases" dataType="ArrayOfMessageContainerBase" optype="categorical">
  <Apply function="GetArray">
    <FieldRef field="gCtx"/>
      <Constant dataType="string">system.SuspectAuctionZipRanges</Constant>
  </Apply>
</DerivedField>

<!--
  Obtain the SuspectAuctionZipRange used to form its filter set (downcast ContainerBase trait elements to ArrayOfZipRange)
-->
<DerivedField name="SuspectAuctionZipRanges" dataType="ArrayOfIntRange" optype="categorical">
  <Apply function="DownCastArrayMembers">
    <FieldRef field="SuspectAuctionZipRangeBases"/>
      <Constant dataType="mbrTypename">IntRange</Constant>
  </Apply>
</DerivedField>



The filter would look like this:  

<DerivedField name="isBadAuction1" dataType="boolean" optype="categorical">
    <Apply function="FoundInAnyOfTheseRanges">
        <FieldRef field="msg.VNZIP"/>
        <FieldRef field="SuspectAuctionZipRanges"/>
        <Constant>true</Constant> <!-- inclusive range compare -->
    </Apply>
</DerivedField>


Something similar would be done for the 2nd rule where there is less confidence ... namely 

Rule number: 6 [IsBadBuy=0 cover=556 (1%) prob=0.21]
WheelTypeID=NULL
TFC_VNZIP1=(27407,27542],(29461,29532],(30212,30272],(30272,30315],(30315,30331],(30529,32124],(32750,32772],(32812,32824],(33311,33314],(33314,33411],(33619,33762],(34203,34761],(35613,37122],(37138,37210],(37210,37421],(39402,42104],(43207,45005],(75020,75050],(75061,75236],(75236,76040],(77301,78219],(78610,78745],(83716,84087],(85009,85018],(87109,89120],(89139,89165],(91763,91770],(92807,94544],(95673,97060],(97060,97217],(97402,98064]


We don't have a function like "FoundInAnyOfTheseRanges" but it would be trivial to add it to the user defined functions (UDFs).

The RuleSet portion would then have three rules:

<RuleSet defaultScore="0">  
<RuleSelectionMethod criterion="firstHit"/>
<SimpleRule id="CATI_Rule1b" score="7">
  <SimplePredicate field="isBadAuction1" operator="equal" value="true"/>
</SimpleRule>
<SimpleRule id="CATI_Rule1a" score="6">
  <SimplePredicate field="isBadAuction2" operator="equal" value="true"/>
</SimpleRule>
<SimpleRule id="CATII_Rule2" score="2">
  <SimplePredicate field="WheelDeal" operator="equal" value="true"/>  <!-- does WheelTypeID in {0,1,2,3} check -->
</SimpleRule>
</RuleSet> 



