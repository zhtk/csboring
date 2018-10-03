package hello;

import com.ibm.able.AbleException;
import com.ibm.able.rules.AbleRuleSet;
import com.ibm.able.rules.AbleRuleSetImpl;

public class Main {

	public static void main(String[] args){
		try {
			AbleRuleSet ruleset = new AbleRuleSetImpl("My first ruleset");
			ruleset.parseFromARL("rules/Hello.arl");
			ruleset.init();
			ruleset.process();
		} catch (AbleException e) {
			e.printStackTrace();
		}
	}
	
}
