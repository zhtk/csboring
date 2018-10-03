package property;

public class PropertyApp {

	public static void main(String[] args){
		try {
			int n = 5;
			CelebrityAgent celebrity = new CelebrityAgent();
			CopycatBean[] copycats = new CopycatBean[n];
			for (int i = 0; i < n; ++i){
				copycats[i] = new CopycatBean(i);
				celebrity.addPropertyChangeListener(copycats[i]);
			}
			
			Thread.sleep(20000);
			
			celebrity.quitAll();
			for (CopycatBean c : copycats)
				c.quitAll();
		} catch (Exception e) {
			System.err.println("Error in BufferApp");
			e.printStackTrace();
		}
	}
	
}
