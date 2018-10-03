package buffer;

import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleException;

public class ConsumerAgent extends AbleDefaultAgent{
	
	private static final long serialVersionUID = -7233283513577676798L;
	
	public ConsumerAgent() throws AbleException{
		super("Consumer");
		inputBuffer = new Integer[1];
		reset();
		init();
	}

	@Override
	public void init() throws AbleException {
		super.init(); // important, init starts a thread responsible for handling the timer.
	}

	@Override
	public void reset() throws AbleException {
		setDataFlowEnabled(true); // indicates we want to process data in buffers
		setSleepTime(2000); // waking up every two seconds
		setTimerEventProcessingEnabled(true); // processing timer events
	}
	
	@Override
	public void processTimerEvent() throws AbleException {
		process();
	}
	
	@Override
	public void process() throws AbleException {
		processBufferConnections(); // loading data waiting in buffers
		Integer chunk = (Integer)getInputBuffer(0);
		System.out.println("Consumed chunk " + chunk);
	}

}
