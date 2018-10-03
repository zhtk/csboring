package pdd1;

import java.io.IOException;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

public class DistinctMapper extends Mapper<Object, Text, Text, Text> {
	private Text newKey = new Text();
	private Text newValue = new Text();
	
	@Override
	public void map(Object key, Text value, Context context)
			throws IOException, InterruptedException {
		String[] vals = value.toString().split("\t");
		newKey.set(vals[0]);
		newValue.set(vals[1]);
		context.write(newKey, newValue);
	}
}
