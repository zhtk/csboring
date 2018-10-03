package dectree;

import com.ibm.able.AbleException;
import com.ibm.able.agents.AbleRuleAgent;
import com.ibm.able.agents.AbleRuleAgentImpl;

public class DiseaseTestApp {
	
	public static void main(String[] args){
		try {
			AbleRuleAgent agent = new AbleRuleAgentImpl("Doctor Agent");
			agent.setBehaviorRuleSetFileName("rules/DiseaseTest.arl");
			agent.init();
			
			Integer age = 36;
			Double temp = 37.1;
			Integer sysBP = 115;
			
			agent.setInputBuffer(0, age);
			agent.setInputBuffer(1, temp);
			agent.setInputBuffer(2, sysBP);
			agent.process();
			System.out.println(agent.getOutputBuffer(0));
			
			agent.quitAll();			
		} catch (AbleException e) {
			e.printStackTrace();
		}	
	}

}
