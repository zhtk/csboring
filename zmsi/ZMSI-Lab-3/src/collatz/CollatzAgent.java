package collatz;

import com.ibm.able.*;
import com.ibm.able.beans.mdp.Pair;

public class CollatzAgent extends AbleDefaultAgent {
	private static final long serialVersionUID = -4478489102840435399L;

	public CollatzAgent() throws AbleException {
		super("Collatz");
		reset();
		init();
	}

	@Override
	public void reset() throws AbleException {
        setAbleEventProcessingEnabled(Able.ProcessingEnabled_PostingEnabled);
	}
	
	@Override
	public void processAbleEvent(AbleEvent evt) throws AbleException {
		Pair<Integer, Integer> msg = (Pair<Integer, Integer>) evt.getArgObject();
		
		int res;
		if (msg.getFirst() % 2 == 0)
			res = msg.getFirst() / 2;
		else
			res = msg.getFirst() * 3 + 1;
		
		if (res == 1)
			System.out.println("Processing done in " + Integer.toString(msg.getSecond()) + " steps.");
		else
			notifyAbleEventListeners(new AbleEvent(this, new Pair<>(res, msg.getSecond() + 1)));
	}
}
