package pdd1;

import java.io.*;
import java.util.*;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Reducer;
import org.slf4j.*;

public class Reductor extends Reducer<Text, Text, Text, DoubleWritable> {
	private boolean isJaccard;
	private DoubleWritable distance = new DoubleWritable();
	private Text resultKey = new Text();
	private Logger logger;
	
	private double getJaccardSimilarity(String t1, String t2) {
		Set<String> shingles1 = MapTask.getShingles(t1);
		Set<String> shingles2 = MapTask.getShingles(t2);
		
		Set<String> union = new HashSet<>(shingles1);
		union.addAll(shingles2);
		
		Set<String> intersection = new HashSet<>(shingles1);
		intersection.retainAll(shingles2);
		
		return intersection.size() / (double) union.size();
	}
	
	private double getHammingDistance(String t1, String t2) {
		int result = 0;
		int size = t1.length() < t2.length() ? t1.length() : t2.length();
		
		for (int i = 0; i < size; ++i)
			if (t1.charAt(i) != t2.charAt(i))
				++result;
		
		return result;
	}
	
	@Override
	public void setup(Context context) throws IOException,
	    InterruptedException {
		Configuration conf = context.getConfiguration();
		isJaccard = conf.getBoolean("similarity.jaccard", true);
		
		logger = LoggerFactory.getLogger(this.getClass());
		logger.debug("Is using Jaccard distance function: {}", isJaccard);
	}
	
	public static int extractRow(String csvLine) {
		int pos = csvLine.indexOf(',');
		return Integer.parseInt(csvLine.substring(0, pos));
	}
	
	public static String extractTweet(String csvLine) {
		int pos = csvLine.indexOf(',');
		return csvLine.substring(pos + 1, csvLine.length() - 1);
	}
	
	@Override
	public void reduce(Text key, Iterable<Text> values, Context context)
			throws IOException, InterruptedException {
		ArrayList<String> tweets1 = new ArrayList<>();
		ArrayList<String> tweets2 = new ArrayList<>();
		
		for (Text val : values) {
			String line = val.toString();
			int num = extractRow(line);
			
			if (num % 2 == 0)
				tweets1.add(line);
			else
				tweets2.add(line);
		}
		
		logger.debug("Parsing key: {}, value size: {} and {}", key.toString(),
				tweets1.size(), tweets2.size());
		
		for (String t1 : tweets1) {
			String cont1 = extractTweet(t1);
			int row1 = extractRow(t1);
			
			for (String t2 : tweets2) {
				String cont2 = extractTweet(t2);
				int row2 = extractRow(t2);
				
				if (isJaccard)
					distance.set(getJaccardSimilarity(cont1, cont2));
				else
					distance.set(getHammingDistance(cont1, cont2));
				
				resultKey.set("[" + row1 + ", " + row2 + "]");
				context.write(resultKey, distance);
			}
		}
	}
}
