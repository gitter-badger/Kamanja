package com.ligadata.Serialize;

import com.google.protobuf.GeneratedMessage;
import com.google.protobuf.ExtensionRegistry;

import com.ligadata.Serialize.MetadataObjects.*;
import com.ligadata.Serialize.MetadataObjects.MetadataType.*;
import java.io.*;

class TestMetadata{
    private static void TestProtoBaseElem(String nameSpace,String name, int version){
	try {
	    ProtoBaseElem be = ProtoBaseElem.newBuilder()
		.setNameSpace(nameSpace)
		.setName(name)
		.setVer(version)
		.build();
	    byte[] ba = be.toByteArray();
	    System.out.println("byte stream size => " + ba.length);
 
	    ProtoBaseElem beFromByteArray = ProtoBaseElem.parseFrom(ba);
	    System.out.println(beFromByteArray);
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }


    private static void TestAttribute(ProtoBaseElem be,ProtoBaseTypeKey bt,String colType){
	try {
	    Attribute a = Attribute.newBuilder()
		.setPbe(be)
		.setPbt(bt)
		.setCollectionType(colType)
		.build();

	    byte[] ba = a.toByteArray();
	    System.out.println("byte stream size => " + ba.length);
 
	    Attribute a1 = Attribute.parseFrom(ba);
	    System.out.println(a1);
 
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }


    public static void main(String[] args) {

	// we need to register all the extensions and tell the decoder about them
	// otherwise the extension will be ignored
	ExtensionRegistry registry= ExtensionRegistry.newInstance();
	MetadataObjects.registerAllExtensions(registry);

	// Test ProtoBaseElem
	TestProtoBaseElem("sys","my_object",100);

	ProtoBaseElem pbe = ProtoBaseElem.newBuilder()
	    .setNameSpace("sys")
	    .setName("my_object")
	    .setVer(100)
	    .build();

	ProtoBaseTypeKey pbt = ProtoBaseTypeKey.newBuilder()
	    .setNameSpace("sys")
	    .setName("int")
	    .setVer(100)
	    .build();

	// Test Attribute
	TestAttribute(pbe,pbt,"Array");
    }
}
