package property;

import com.ibm.able.AbleDefaultAgent;
import com.ibm.able.AbleException;

public class CelebrityAgent extends AbleDefaultAgent{

	private static final long serialVersionUID = 7718228592613085111L;
	
	private static final String[] COLORS = new String[]{"black", "white", "red", "pink", "blue"};
	
	private int colorIndex;
	public String color;
	
	public CelebrityAgent() throws AbleException{
		colorIndex = 0;
		color = COLORS[colorIndex];
		reset();
		init();
	}
	
	public String getColor(){
		return color;
	}
	
	private void nextColor(){
		String oldColor = color;
		colorIndex = (colorIndex + 1) % COLORS.length;
		color = COLORS[colorIndex];
		firePropertyChange("color", oldColor, color);
	}

	@Override
	public void reset() throws AbleException {
		setSleepTime(2000); // waking up every 2 seconds
		setTimerEventProcessingEnabled(true); // processing timer events
	}
	
	@Override
	public void processTimerEvent() throws AbleException {
		process();
	}

	@Override
	public void process() throws AbleException {
		nextColor();
		System.out.println("This season " + color + " is glamour.");
	}

}
