This project contains a Sample UDF file in it that may be used as a model for your own Custom UDF work.

The basic rules for custom udfs:

1) All functions need to return a value.
2) The functions must be static (i.e., appear in a Scala object, not a class)
3) The package can be anything you like.  It however must be specified in your models
in the DataDictionary DataField named UDFSearchPath as a value.  See CustomUdfs.scala in this
project for more information.
4) It is no longer required to inherit the UdfBase trait.  It is scheduled for removal and
has been marked deprecated.  Make adjustments to your Udf library to prepare for this
eventuality.
5) Your first best go to documentation resource is the developer guide in the wiki.


