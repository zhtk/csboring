package events;

import com.ibm.able.Able;
import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleEvent;
import com.ibm.able.AbleException;

public class BrokerAgent extends AbleDefaultAgent{

	private static final long serialVersionUID = -4478489102840435388L;
	
	public BrokerAgent() throws AbleException{
		super("Broker");
		reset();
		init();
	}

	@Override
	public void init() throws AbleException {
		super.init(); // important, init starts a thread responsible for handling the timer.
	}

	@Override
	public void reset() throws AbleException {
        setAbleEventProcessingEnabled(Able.ProcessingEnabled_PostingEnabled); // important, processing events
	}
	
	public void makeTransaction(InvestorAgent.TransactionMessage msg){
		if (msg.getAmount() >= 0)
			System.out.println(Thread.currentThread().getId() +": Buing " + msg.getAmount() + " gallons");
		else
			System.out.println(Thread.currentThread().getId() +": Selling " + Math.abs(msg.getAmount()) + " gallons");
	}
	
	@Override
	public void processAbleEvent(AbleEvent evt) throws AbleException {
		InvestorAgent.TransactionMessage msg = (InvestorAgent.TransactionMessage) evt.getArgObject();
		makeTransaction(msg);
	}

}
