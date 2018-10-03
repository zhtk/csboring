package buffer;

import java.util.Random;

import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleException;

public class ProducerAgent extends AbleDefaultAgent {

	private static final long serialVersionUID = 7804030118314649187L;

	private static final Random RAND = new Random();

	public ProducerAgent() throws AbleException{
		super("Producer");
		outputBuffer = new Integer[1];
		reset();
		init();
	}

	@Override
	public void init() throws AbleException {
		super.init(); // important, init starts a thread responsible for handling the timer.
	}

	@Override
	public void reset() throws AbleException {
		setDataFlowEnabled(true);  // indicates we want to process data in buffers
		setSleepTime(2000); // waking up every two seconds
		setTimerEventProcessingEnabled(true); // processing timer events
	}
	
	@Override
	public void processTimerEvent() throws AbleException {
		process();
	}

	@Override
	public void process() throws AbleException {
		Integer chunk = RAND.nextInt(1000);
		setOutputBuffer(0, chunk);
		System.out.println("Produced chunk " + chunk);
	}

}
