package com.ligadata.kamanja.CheckerComponent;

import java.io.BufferedReader;
import java.io.FileReader;

public class FileUtility {
	String logFilePath;
	String fileContent = "";

	public void ReadLogFile() {
		logFilePath = System.getProperty("java.io.tmpdir") + "/Get-Component.log";
		String line = null;

		try {
			FileReader fileReader = new FileReader(logFilePath);
			BufferedReader bufferedReader = new BufferedReader(fileReader);

			while ((line = bufferedReader.readLine()) != null) {
				fileContent += line + "\n";
			}
			bufferedReader.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public String getLogFilePath() {
		return logFilePath;
	}

	public void setLogFilePath(String logFilePath) {
		this.logFilePath = logFilePath;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}
}
