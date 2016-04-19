package com.ligadata.kamanja.CheckerComponent;
//package check_prerequisites;
//
//import org.eclipse.debug.ui.console.*;
//import org.eclipse.jface.text.IRegion;
//
//public class LogTracker implements IConsoleLineTracker {
//
//	private IConsole m_console;
//
//	public void dispose() {
//
//	}
//
//	public void init(IConsole console) {
//		m_console = console;
//	}
//
//	public void lineAppended(IRegion region) {
//		try {
//			String line = m_console.getDocument().get(region.getOffset(), region.getLength());
//			System.out.println("\nhi\n" + line.substring(0, 10));
//			// DO SOMETHING WITH THAT LINE
//		} catch (Exception e) {
//			// WrCheck.logError(e);
//			e.printStackTrace();
//		}
//	}
//}