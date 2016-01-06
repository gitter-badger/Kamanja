package com.ligadata.adapters.pipeline;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import com.ligadata.adapters.scratch.CommandLineUtils;
import com.ligadata.adapters.utility.WrappedScheduleExecutor;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class PipelineRunner {

	public static void main(String[] args){
		Options options = null;
		try{
			options = new Options();
			OptionGroup oGroup = new OptionGroup();
			Option opt = Option.builder("c")
					.required(true)
					.longOpt("config")
					.desc("Location of Config File")
					.argName("Config File Location")
					.hasArg(true)
					.build();
			oGroup.addOption(opt);
			options.addOptionGroup(oGroup);
			
			CommandLineParser cmdParser = new DefaultParser();
			CommandLine cmdLine = cmdParser.parse( options, args);
	
			String fileLoc = null;
			if(cmdLine != null && cmdLine.hasOption("c")) {
				fileLoc = cmdLine.getOptionValue("c");
			}
			
			File f = new File(fileLoc);
			if(f.exists()){
				BufferedReader reader = new BufferedReader(new FileReader(f));
				StringBuilder b = new StringBuilder();
				String line = reader.readLine();
				while(line != null){
					b.append(line);
					line = reader.readLine();
				}
				System.out.println(b.toString());
				
				JSONParser parser = new JSONParser();
				JSONObject config = null;
		
				config =  (JSONObject)parser.parse(b.toString());
				
				boolean runOnce=false;
				long interval = (Long)config.get("runInterval");
				String timeunit = (String)config.get("runUnits");
				
				TimeUnit tu = TimeUnit.valueOf(timeunit.toUpperCase());
				
				IPipeline command = new Pipeline();
				command.create(config);
				
				if(runOnce){
					Thread thread = new Thread(command);
					try{
						thread.start();
						thread.join();
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					command.close();
				}else{
					WrappedScheduleExecutor scheduler = new WrappedScheduleExecutor(1);
					scheduler.scheduleAtFixedRate(command, 0, interval, tu);
					//Need to decide where to close resources
				}
			}else{
				log.error("Invalid Options....");
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp( "java -jar com.ligadata.adapters.pipeline.PipelineRunner", options );
			}
		}catch(IOException | org.apache.commons.cli.ParseException | ParseException ex){
			log.error("Invalid Options...."+ex.getMessage());
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp( "java -jar com.ligadata.adapters.pipeline.PipelineRunner", options );
		}
	}
}

//Run Parameter Arguments
//-c tableRunnerConfig1.json
//-c queryRunnerConfig1.json
