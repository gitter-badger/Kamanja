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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

import org.joda.time.DateTime;
import org.joda.time.Days;

public class RandomHelper {

	private static Random RandomSeed = new Random();
	
	private static char[] Symbols;
	private static char[] CapitalSymbols;

	static {
	    StringBuilder tmp = new StringBuilder();
	    StringBuilder tmpCap = new StringBuilder();
	    for (char ch = '0'; ch <= '9'; ++ch)
	      tmp.append(ch);
	    for (char ch = 'a'; ch <= 'z'; ++ch)
	      tmp.append(ch);
	    for (char ch = 'A'; ch <= 'Z'; ++ch){
	    	tmp.append(ch);
	    	tmpCap.append(ch);
	    }
	    Symbols = tmp.toString().toCharArray();
	    CapitalSymbols = tmpCap.toString().toCharArray();
	  }
	
	private static double NextFloatingNumber(){
		
		return RandomSeed.nextDouble();
	}
	
	public static int GenerateInteger(int Length){
		
		long range = (long) Math.pow(10, Length);
		
		int value = (int) Math.round(Math.abs(NextFloatingNumber()) * range);
		
		return value > 0 ? value : -1 * value; 
	}
	
	public static long GenerateLong(int Length){
		
		long range = (long) Math.pow(10, Length);
		
		long value = Math.round(Math.abs(NextFloatingNumber()) * range);
		
		return value > 0 ? value : -1 * value; 
	}
	
	public static int GenerateIntegerFromRange(int min, int max){
		
		int range = max - min;
		
		int value = (int) Math.round(Math.abs(NextFloatingNumber()) * range);
		
		return (value > 0 ? value : -1 * value) + min;
	}
	
	public static double GenerateDecimal(int Length, int DecimalLength){
		
		long range = (long) Math.pow(10, Length);
		
		double num = Math.abs(NextFloatingNumber()) * range;
				
		double value = new BigDecimal(num).setScale(DecimalLength, RoundingMode.HALF_UP).doubleValue();
		
		return value > 0 ? value : value * -1;
	}
	
	public static String GenerateString(int Length){
		
		char[] buf = new char[Length];
		
		for (int idx = 0; idx < buf.length; ++idx) 
		      buf[idx] = Symbols[RandomSeed.nextInt(Symbols.length)];
	    
		return new String(buf);
	}
	
	public static char GenerateChar(){
		
		return CapitalSymbols[RandomSeed.nextInt(CapitalSymbols.length)];
	}
	
	public static String GenerateMobile(){
		
		// Mobile format (XXX) XXX-XXXX
		
		String firstPart = GenerateInteger(1) + "" + GenerateInteger(1) + GenerateInteger(1);
		String secondPart = GenerateInteger(1) + "" + GenerateInteger(1) + GenerateInteger(1);
		String thirdPart = GenerateInteger(1) + "" + GenerateInteger(1) + GenerateInteger(1) + GenerateInteger(1);
		
		return "(" + firstPart + ") " + secondPart + "-" + thirdPart;
	}
	
	public static DateTime GenerateDate(DateTime startDate, DateTime endDate){
		
		int difference = Days.daysBetween(endDate, startDate).getDays();
		
		int newDays = GenerateIntegerFromRange(0, difference);
		
		return startDate.plusDays(newDays);
	}

	public static String GeneratePreciseTime(){
		
		String time = "";
		
		int HH = GenerateInteger(2) % 23;
		int MM = GenerateInteger(2) % 60;
		int SS = GenerateInteger(2) % 60;
		int CC = GenerateInteger(2);
		
		time = (HH >= 10) ? ("" + HH) : ("0" + HH);
		time += (MM >= 10) ? ("" + MM) : ("0" + MM);
		time += (SS >= 10) ? ("" + SS) : ("0" + SS);
		time += (CC >= 10) ? ("" + CC) : ("0" + CC);
		
		return time;
	}
}
