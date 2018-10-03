package task3;

import com.ibm.able.AbleEvent;

public class DetectorApp {
	public static void main(String[] args){
		try {
			// Zdarzenia do przerobienia
			Email[] events = new Email[]{
				new Email("spammer", "subject", "This message is SPAM!"),
				new Email("nospammer", "subject2", "content"),
				new Email("nosaspr", "", "wrong: this is empty subject"),
				new Email("dealmaker", "make great deal!!!", "BUY NOW my honey pot!!!!1")
			};
			
			DetectorAgent agent = new DetectorAgent();
			agent.addAbleEventListener(agent);
			
			for (Email e : events) {
				agent.notifyAbleEventListeners(new AbleEvent(agent, e));
				Thread.sleep(500);
			}
			
			agent.quitAll();
		} catch (Exception e) {
			System.err.println("Error in DetectorApp");
			e.printStackTrace();
		}
	}
}
