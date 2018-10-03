package buffer;

import com.ibm.able.AbleBufferConnection;

public class BufferApp {

	public static void main(String[] args){
		try {
			ProducerAgent producer = new ProducerAgent();
			ConsumerAgent consumer = new ConsumerAgent();
			new AbleBufferConnection(producer,consumer); //creating a new connection between two buffers
			
			Thread.sleep(20000);
			
			producer.quitAll();
			consumer.quitAll();
		} catch (Exception e) {
			System.err.println("Error in BufferApp");
			e.printStackTrace();
		}
	}
	
}
