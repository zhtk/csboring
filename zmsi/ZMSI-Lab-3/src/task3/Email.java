package task3;

public class Email {
	public final String from;
	public final String title;
	public final String content;
	
	public Email(String from, String title, String content) {
		this.from = from;
		this.title = title;
		this.content = content;
	}
	
	public String toString() {
		return from + ", " + title + ", "+ content;
	}
}
