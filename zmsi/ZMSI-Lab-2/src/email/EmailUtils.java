package email;

public class EmailUtils {
	public boolean nonEmptyTitle(Email e) {
		return !e.title.equals("");
	}
	
	public boolean isUnknownSender(Email e) {
		String[] known = new String[]{
			"Jane",
			"Bob",
			"Robert"
		};
		
		for (String s : known)
			if (s.equals(e.from))
				return false;
		
		return true;
	}
	
	public boolean isSpammer(Email e) {
		String[] spammer = new String[]{
			"spammer",
			"SpamHouse",
			"DealMaker"
		};
		
		for (String s : spammer)
			if (s.equals(e.from))
				return true;
		
		return false;
	}
	
	public boolean isAnDealOffer(Email e) {
		return e.content.contains("BUY NOW") || e.content.contains("OFFER") || 
		       e.content.contains("DEAL");
	}
}
