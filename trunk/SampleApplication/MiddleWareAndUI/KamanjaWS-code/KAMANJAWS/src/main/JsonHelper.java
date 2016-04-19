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

import java.util.List;

import org.apache.commons.lang3.StringEscapeUtils;

import flexjson.JSONSerializer;

public class JsonHelper 
{
	public static String SerializeList(Object obj) 
	{
		return SerializeList(obj, true, true, null).replaceAll("\'", "\u0027");
	}
	
	public static String SerializeList(Object obj, boolean escape, boolean wrapUpdInXml, List<String> excludeAttributes) 
	{
		
		JSONSerializer serializer = new JSONSerializer();
		
		if(excludeAttributes!=null && excludeAttributes.size()>0)
			serializer.setExcludes(excludeAttributes);
		
		String serializedValue = (serializer.exclude("*.class").deepSerialize(obj).replaceAll("\"class\"", "\"clazz\""));
		
		//.replaceAll("\"", "&quot;")
		if(wrapUpdInXml){
			if(escape)
				return wrapUpInXml(StringEscapeUtils.escapeXml(serializedValue));
			else
				return wrapUpInXml(serializedValue);
		}
		else{
			if(escape)
				return (StringEscapeUtils.escapeXml(serializedValue));
			else
				return (serializedValue);
		}
	}
	
	public static String wrapUpInXml(String src)
	{
		return "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
				+ "<string xmlns=\"http://tempuri.org/\">" + src + "</string>";
	}
}
