package email;

import com.ibm.able.Able;
import com.ibm.able.AbleException;
import com.ibm.able.rules.AbleRuleSet;
import com.ibm.able.rules.AbleRuleSetImpl;

public class SpamDetectionApp {

	public static void main(String[] args){
		try	{
			//retrieve the rule set
			String rules = "rules/Email.arl";
			AbleRuleSet ruleSet = new AbleRuleSetImpl();
			ruleSet.parseFromARL(rules);
			ruleSet.init();
			
			// create our data
			Email[] data = new Email[]{
					new Email("spammer", "subject", "This message is SPAM!"),
					new Email("nospammer", "subject2", "content"),
					new Email("nosaspr", "", "wrong: this is empty subject"),
					new Email("dealmaker", "make great deal!!!", "BUY NOW my honey pot!!!!1")
			};
			
			Object[] input = null;
			Object[] output = null;
			
			Able.startTraceLogging(Able.TRC_LOW, Able.MSG_NONE, null);
			
			for (int i=0; i<data.length; i++){
				output = (Object[]) ruleSet.process(input = new Object[]{data[i]});			
				Able.TraceLog.text(Able.TRC_LOW, "Input  result: " + displayBuffer(input));
				Able.TraceLog.text(Able.TRC_LOW, "Output result: " + displayBuffer(output));
			}
		} catch (AbleException ae){
			if (ae.getExceptions() == null)
				System.err.println(ae.getLocalizedMessage());
			else
				for (Object o : ae.getExceptions()){
					Exception e = (Exception) o;
					System.err.println(e.getLocalizedMessage());
				}
		} catch (Exception e){
			System.err.println(e.getLocalizedMessage());
			e.printStackTrace();
		}
	}

	private static String displayBuffer(Object[] buf){
		StringBuffer buffer = new StringBuffer("");
		if (buf == null)
			buffer.append(buf);
		else
			for (int i = 0; i < buf.length; i++)
				buffer.append(buf[i] + " ");    
		return buffer.toString();    
	}
}