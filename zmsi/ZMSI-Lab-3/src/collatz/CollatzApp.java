package collatz;

import com.ibm.able.AbleEvent;
import com.ibm.able.beans.mdp.Pair;

public class CollatzApp {
	public static void main(String[] args){
		try {
			int n = 10;
			int starting = 10;
			
			CollatzAgent[] agents = new CollatzAgent[n];
			for (int i = 0; i < n; ++i)
				agents[i] = new CollatzAgent();
			for (int i = 0; i < n; ++i)
				agents[i].addAbleEventListener(agents[(i + 1) % n]);
			
			agents[0].notifyAbleEventListeners(new AbleEvent(agents[0], new Pair<>(starting, 1)));
			
			Thread.sleep(20000);
			
			for (CollatzAgent a : agents)
				a.quitAll();
		} catch (Exception e) {
			System.err.println("Error in CollatzApp");
			e.printStackTrace();
		}
	}
}
