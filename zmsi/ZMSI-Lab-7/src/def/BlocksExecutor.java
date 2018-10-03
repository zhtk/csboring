package def;

public class BlocksExecutor {

	public static boolean pickup(String x){
		System.out.println("JAVA: Picking up block " + x);
		return true;
	}
	
	public static boolean putdown(String x){
		System.out.println("JAVA: Putting down block " + x);		
		return true;
	}
	
	public static boolean stack(String x, String z){
		System.out.println("JAVA: Stacking block " + x + " on block " + z);
		return true;
	}
	
	public static boolean unstack(String x, String z){
		System.out.println("JAVA: Unstacking block " + x + " from block " + z);
		return true;
	}
}