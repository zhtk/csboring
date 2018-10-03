package riddles;


public class RiddleUtils {

	private String[] colors = {"white", "black", "red", "blue", "green", "yellow", "orange", "grey", "golden", "silver", "voliet", "pink"};
	private String[] size = {"small", "medium", "large", "huge", "little"};
	private String[] shape = new String[] {"rectangle", "square", "triangle", "circle", "oval"};
	private String[] location = new String[] {"house", "water", "air", "room"};
	private String[] verb = new String[] {"swim", "move", "run", "shine", "fly"};

	public boolean isBlankOrNull(String string){
		if ( string==null || string.trim().equals("") )
			return true;
		else
			return false;
	}

	public boolean containsColor(String string){
		for (String c : colors)
			if ( string.contains(c) )
				return true;
		return false;
	}


	public String extractColor(String string){
		for (String c : colors)
			if ( string.contains(c) )
				return c;
		return null;
	}


	public String extractSize(String string){
		for (String c : size)
			if ( string.contains(c) )
				return c;
		return null;
	}


	public String extractShape(String string){
		for (String c : shape)
			if ( string.contains(c) )
				return c;
		return null;
	}

	public String extractLocation(String string){
		for (String c : location)
			if ( string.contains(c) )
				return c;
		return null;
	}

	public String extractVerb(String string){
		for (String c : verb)
			if ( string.contains(c) )
				return c;
		return null;
	}

}
