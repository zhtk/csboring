package census;

import java.io.Serializable;

public class Person implements Serializable {

	static final long serialVersionUID = 2001100500100300000L;

	private double age;
	private String workclass;
	private double fnlwgt;
	private String education;
	private double education_num;
	private String marital_status;
	private String occupation;
	private String relationship;
	private String race;
	private String sex;
	private double capital_loss;
	private double capital_gain;
	private double hours_per_week;
	private String native_country;
	private String income;

	public Person() {}

	public Person(double age, String workclass, double fnlwgt, String education,
			double education_num, String marital_status, String occupation,
			String relationship, String race, String sex, double capital_gain,
			double capital_loss, double hours_per_week, String native_country, String income) {
		this.age = age;
		this.workclass = workclass;
		this.fnlwgt = fnlwgt;
		this.education = education;
		this.education_num = education_num;
		this.marital_status = marital_status;
		this.occupation = occupation;
		this.relationship = relationship;
		this.race = race;
		this.sex = sex;
		this.capital_gain = capital_gain;
		this.capital_loss = capital_loss;
		this.hours_per_week = hours_per_week;
		this.native_country = native_country;
		this.income = income;
	}

	public String toString() {
		return 
				"age="+ age  + ", " +
				"workclass="+ workclass  + ", " +
				"fnlwgt="+ fnlwgt  + ", " +
				"education="+ education  + ", " +
				"education_num="+ education_num  + ", " +
				"marital_status="+ marital_status  + ", " +
				"occupation="+ occupation  + ", " +
				"relationship="+ relationship  + ", " +
				"race="+ race  + ", " +
				"sex="+ sex  + ", " +
				"capital_gain="+ capital_gain  + ", " +
				"capital_loss="+ capital_loss  + ", " +
				"hours_per_week="+ hours_per_week  + ", " +
				"native_country="+ native_country + ", " + 
				"income=" + income;
	}

	public double getAge() {
		return age;
	}

	public void setAge(String age) {
		this.age = Double.parseDouble(age);
	}

	public String getWorkclass() {
		return workclass;
	}

	public void setWorkclass(String workclass) {
		this.workclass = workclass;
	}

	public double getFnlwgt() {
		return fnlwgt;
	}

	public void setFnlwgt(String fnlwgt) {
		this.fnlwgt =  Double.parseDouble(fnlwgt);
	}

	public String getEducation() {
		return education;
	}

	public void setEducation(String education) {
		this.education = education;
	}

	public double getEducation_num() {
		return education_num;
	}

	public void setEducation_num(String education_num) {
		this.education_num =  Double.parseDouble(education_num);
	}

	public String getMarital_status() {
		return marital_status;
	}

	public void setMarital_status(String marital_status) {
		this.marital_status = marital_status;
	}

	public String getOccupation() {
		return occupation;
	}

	public void setOccupation(String occupation) {
		this.occupation = occupation;
	}

	public String getRelationship() {
		return relationship;
	}

	public void setRelationship(String relationship) {
		this.relationship = relationship;
	}

	public String getRace() {
		return race;
	}

	public void setRace(String race) {
		this.race = race;
	}

	public String getSex() {
		return sex;
	}

	public void setSex(String sex) {
		this.sex = sex;
	}

	public double getCapital_gain() {
		return capital_gain;
	}

	public void setCapital_gain(String capital_gain) {
		this.capital_gain =  Double.parseDouble(capital_gain);
	}

	public double getCapital_loss() {
		return capital_loss;
	}

	public void setCapital_loss(String capital_loss) {
		this.capital_loss =  Double.parseDouble(capital_loss);
	}

	public double getHours_per_week() {
		return hours_per_week;
	}

	public void setHours_per_week(String hours_per_week) {
		this.hours_per_week =  Double.parseDouble(hours_per_week);
	}

	public String getNative_country() {
		return native_country;
	}

	public void setNative_country(String native_country) {
		this.native_country = native_country;
	}
	
	public String getIncome() {
		return income;
	}
	
	public void setIncome(String income) {
		this.income = income;
	}

}
