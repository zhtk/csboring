package events;

import java.util.Random;

import com.ibm.able.Able;
import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleEvent;
import com.ibm.able.AbleException;

public class NewsAgencyAgent extends AbleDefaultAgent {

	private static final long serialVersionUID = 7989074649525317891L;

	private static final Random RAND = new Random();

	public NewsAgencyAgent() throws AbleException{
		super("News agency");
		reset();
		init();
	}

	@Override
	public void init() throws AbleException {
		super.init(); // important, init starts a thread responsible for handling the timer.
	}

	@Override
	public void reset() throws AbleException {
		setSleepTime(2000); // waking up every two seconds
		setTimerEventProcessingEnabled(true); // processing timer events
        setAbleEventProcessingEnabled(Able.ProcessingDisabled_PostingEnabled); // important, publishing events
	}
	
	@Override
	public void processTimerEvent() throws AbleException {
		process();
	}

	@Override
	public void process() throws AbleException {
		Integer price = RAND.nextInt(1000);
		String text = "Oil prices are " + price + " USD per gallon";
		OilPricesMessage msg = new OilPricesMessage(text, price);
		System.out.println("\n" + Thread.currentThread().getId() + ": Broadcasting message: " + text);
		notifyAbleEventListeners(new AbleEvent(this, msg)); // asynchronous call
	}
	
	public static class OilPricesMessage {
		private String text;
		private Integer price;
		
		public OilPricesMessage(String text, Integer price) {
			super();
			this.text = text;
			this.price = price;
		}
		
		public String getText() {
			return text;
		}
		
		public Integer getPrice() {
			return price;
		}
	}

}
