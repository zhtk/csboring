package events;


public class EventsApp {

	public static void main(String[] args){
		try {
			int n = 3;
			NewsAgencyAgent newsAgency = new NewsAgencyAgent();
			InvestorAgent[] investors = new InvestorAgent[n];
			BrokerAgent[] brokers = new BrokerAgent[n];
			for (int i = 0; i < n; ++i){
				investors[i] = new InvestorAgent();
				newsAgency.addAbleEventListener(investors[i]);
				brokers[i] = new BrokerAgent();
				investors[i].addAbleEventListener(brokers[i]);
			}
			
			Thread.sleep(20000);
			
			newsAgency.quitAll();
			for (InvestorAgent a : investors)
				a.quitAll();
			for (BrokerAgent a : brokers)
				a.quitAll();
		} catch (Exception e) {
			System.err.println("Error in BufferApp");
			e.printStackTrace();
		}
	}
	
}
