package property;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import com.ibm.able.AbleObject;

public class CopycatBean extends AbleObject implements PropertyChangeListener {

	private static final long serialVersionUID = -6108282092948978081L;
	
	private String color;

	public CopycatBean(int i) {
		super("Copycat no. " + i);
		color = "gray";
	}

	public void setColor(String c) {
		color = c;
		System.out.println("From now on I wear only " + color + "!");
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		setColor((String)evt.getNewValue());
	}
}
