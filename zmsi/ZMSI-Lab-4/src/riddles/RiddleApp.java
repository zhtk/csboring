package riddles;

import java.util.Scanner;

import com.ibm.able.AbleException;
import com.ibm.able.rules.AbleParException;
import com.ibm.able.rules.AbleRuleSet;
import com.ibm.able.rules.AbleRuleSetImpl;

public class RiddleApp {

	public static void main(String[] args){
		
		Scanner sc = new Scanner(System.in);
		String line = null;
		while (true){
			System.out.println("Enter riddle or END to quit:");
			line = sc.nextLine();
			if ("END".equals(line))
				break;
			String result = null;
			
			
			try {
				AbleRuleSet ruleSet = new AbleRuleSetImpl();
				ruleSet.parseFromARL("rules/riddle.arl");
				ruleSet.init();
				Object[] outs = (Object[]) ruleSet.process(new String[]{line});
				result = (String) outs[0];
			} catch (Exception e) {
				e.printStackTrace();
			}
			
			System.out.println("Result is " + result);
		}
		sc.close();
		
	}

}
