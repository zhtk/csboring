package task3;

import com.ibm.able.*;
import com.ibm.able.rules.AbleRuleSet;
import com.ibm.able.rules.AbleRuleSetImpl;

public class DetectorAgent extends AbleDefaultAgent {
	private static final long serialVersionUID = -4478483402840435399L;
	private final AbleRuleSet ruleSet = new AbleRuleSetImpl();
	
	public DetectorAgent() throws AbleException {
		super("Detector");
		reset();
		init();
		
		ruleSet.parseFromARL("rules/Email.arl");
	}

	@Override
	public void reset() throws AbleException {
        setAbleEventProcessingEnabled(Able.ProcessingEnabled_PostingEnabled);
	}
	
	@Override
	public void processAbleEvent(AbleEvent evt) throws AbleException {
		Email msg = (Email) evt.getArgObject();
		String[] output = (String[]) ruleSet.process(new Object[]{msg});

		if (output.length > 0 && output[0] != null)
			System.out.println(output[0].toString() + " detected: " + msg.toString());
	}
}
