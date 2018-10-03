package census;

import java.util.*;

import com.ibm.able.AbleException;
import com.ibm.able.data.*;
import com.ibm.able.rules.*;

public class PersonUtils {
	public String aggregateByRaceMax(Object[] objs) {
		Map<String, Integer> rasy = new HashMap<String, Integer>();
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			int count = 0;
			if (rasy.containsKey(p.getRace()))
				count = rasy.get(p.getRace());
				
			rasy.put(p.getRace(), count + 1);
		}
		
		int max = -1;
		String res = "";
		
		for (Map.Entry<String, Integer> entry: rasy.entrySet())
			if (entry.getValue() > max) {
				res= entry.getKey();
				max = entry.getValue();
			}
		
		return res;
	}
	
	public String aggregateByRaceSex(Object[] objs) {
		Map<String, Integer> rasyM = new HashMap<String, Integer>();
		Map<String, Integer> rasyF = new HashMap<String, Integer>();
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			Map<String, Integer> rasy;
			if (p.getSex().equals("Male"))
				rasy = rasyM;
			else
				rasy = rasyF;
			
			int count = 0;
			if (rasy.containsKey(p.getRace()))
				count = rasy.get(p.getRace());
			
			rasy.put(p.getRace(), count + 1);
		}
		
		String res = "";
		for (String race: rasyF.keySet()) {
			res += race;
			res += " - males " + rasyM.get(race).toString();
			res += ", females " + rasyF.get(race).toString();
			res += "; ";
		}
		
		return res;
	}
	
	public String educationNum(Object[] objs) {
		Map<Double, String> edu = new TreeMap<Double, String>();
		
		for (Object o : objs) {
			Person p = (Person) o;
			edu.put(p.getEducation_num(), p.getEducation());
		}
		
		String res = "";
		for (Map.Entry<Double, String> entry: edu.entrySet()) {
				res += entry.getKey();
				res += " -> ";
				res += entry.getValue();
				res += "; ";
			}
		
		return res;
	}
	
	public String getEduMax(Object[] objs) {
		Map<String, Integer> edu = new HashMap<String, Integer>();
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			int count = 0;
			if (edu.containsKey(p.getEducation()))
				count = edu.get(p.getEducation());
				
			edu.put(p.getEducation(), count + 1);
		}
		
		int max = -1;
		String res = "";
		
		for (Map.Entry<String, Integer> entry: edu.entrySet())
			if (entry.getValue() > max) {
				res = entry.getKey();
				max = entry.getValue();
			}
		
		return res;
	}
	
	public String hoursPerWeek(Object[] objs) {
		Map<String, Double> edu = new TreeMap<String, Double>();
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			double count = 0;
			if (edu.containsKey(p.getOccupation()))
				count = edu.get(p.getOccupation());
			
			if (count < p.getHours_per_week())
				count = p.getHours_per_week();
			
			edu.put(p.getOccupation(), count);
		}
		
		String res = "";
		double max = -1;
		for (Map.Entry<String, Double> entry: edu.entrySet())
			if (entry.getValue() > max) {
				res = entry.getKey();
				max = entry.getValue();
			}
		
		return res;
	}
	
	public String hpwAvg(Object[] objs) {
		double sum = 0;
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			sum += p.getHours_per_week();
		}
		
		return Double.toString(sum / objs.length);
	}
	
	public String lostMost(Object[] objs) {
		double loss = 0;
		Person lp = (Person) objs[0];
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			if (p.getCapital_loss() > loss) {
				loss = p.getCapital_loss();
				lp = p;
			}
		}
		
		return lp.toString();
	}
	
	public String gainMost(Object[] objs) {
		double gain = 0;
		Person lp = (Person) objs[0];
		
		for (Object o : objs) {
			Person p = (Person) o;
			
			if (p.getCapital_gain() > gain) {
				gain = p.getCapital_gain();
				lp = p;
			}
		}
		
		return lp.toString();
	}
	
	public String countriesJob(AbleRuleSet ctx, AbleWorkingMemory wm) throws AbleException {
		String result = "";
		
		for (String country : new String[]{"United-States", "Cambodia",
				"England", "Puerto-Rico", "Canada", "Germany", "Outlying-US(Guam-USVI-etc)",
				"India", "Japan", "Greece", "South", "China", "Cuba", "Iran", "Honduras",
				"Philippines", "Italy", "Poland", "Jamaica", "Vietnam", "Mexico",
				"Portugal", "Ireland", "France", "Dominican-Republic", "Laos",
				"Ecuador", "Taiwan", "Haiti", "Columbia", "Hungary", "Guatemala",
				"Nicaragua", "Scotland", "Thailand", "Yugoslavia", "El-Salvador",
				"Trinadad&Tobago", "Peru", "Hong", "Holand-Netherlands"}) {
			int bestJobCount = 0;
			String bestJob = "";
			
			for (String job : new String[]{"Tech-support", "Craft-repair", "Other-service",
					"Sales", "Exec-managerial", "Prof-specialty", "Handlers-cleaners",
					"Machine-op-inspct", "Adm-clerical", "Farming-fishing", "Transport-moving",
					"Priv-house-serv", "Protective-serv", "Armed-Forces"}) {
				String condition = "person.native_country == \""+country+"\" OR person.occupation == \""+job+"\"";
				AbleExpression expr = new AbleExpression(ctx, condition);  
				AbleSelector selector =	new AbleSelector(ctx.getVariable("person"), expr, true);
				
				int count = wm.findAll(selector).size();
				if (count > bestJobCount) {
					bestJob = job;
					bestJobCount = count;
				}
			}
			
			result += country + " -> " + bestJob + "; ";
		}
		
		return result;
	}
}
