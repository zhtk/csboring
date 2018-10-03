package pdd1;

import java.io.*;
import org.apache.hadoop.mapreduce.Reducer;

public class DistinctReducer extends Reducer<Object, Object, Object, Object> {
	@Override
	public void reduce(Object key, Iterable<Object> values, Context context)
			throws IOException, InterruptedException {
		context.write(key, values.iterator().next());
	}
}
