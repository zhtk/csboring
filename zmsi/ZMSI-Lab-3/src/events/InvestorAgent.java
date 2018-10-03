package events;

import java.util.Random;

import com.ibm.able.Able;
import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleEvent;
import com.ibm.able.AbleException;

public class InvestorAgent extends AbleDefaultAgent{

	private static final long serialVersionUID = 7654298942942467899L;
	
	private static final Random RAND = new Random();
	
	public InvestorAgent() throws AbleException{
		super("Investor");
		reset();
		init();
	}

	@Override
	public void init() throws AbleException {
		super.init(); // important, init starts a thread responsible for handling the timer.
	}

	@Override
	public void reset() throws AbleException {
        setAbleEventProcessingEnabled(Able.ProcessingEnabled_PostingEnabled); // important, publishing events and processing events
	}
	
	@Override
	public void processAbleEvent(AbleEvent evt) throws AbleException {
		NewsAgencyAgent.OilPricesMessage msg = (NewsAgencyAgent.OilPricesMessage) evt.getArgObject();
		Integer amount = RAND.nextInt() % 10000;
		System.out.println(Thread.currentThread().getId() +": Oil prices are " + msg.getPrice() + " sending transaction for " + amount + " gallons");
		notifyAbleEventListeners(new AbleEvent(this, new TransactionMessage(amount), "", false)); // synchronous call
	}

	public static class TransactionMessage {
		private Integer amount;
		
		public TransactionMessage(Integer amount) {
			this.amount = amount;
		}
		
		public Integer getAmount() {
			return amount;
		}
	}
}
