package pdd1;

import java.io.*;
import java.util.*;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Mapper;

public class MapTask extends Mapper<Object, Text, Text, Text> {
	public static final int SHINGLE_LENGTH = 5;
	public static final int PERMUTATIONS = 100;
	public static final int BANDS = 20;
	public static final int BAND_SIZE = 5;
	
	private Configuration conf;
	
	public static String stripSpecialChars(Text text) {
		return text.toString().replaceAll("[^A-Za-z]", "");		
	}
	
	public static Set<String> getShingles(String text) {
		Set<String> result = new HashSet<>();
		
		for (int i = 0; i < text.length() - SHINGLE_LENGTH; ++i)
			try {
				result.add(text.substring(i, i + SHINGLE_LENGTH).toLowerCase());
			} catch(Exception e) {
				System.err.println(e.getMessage());
			}
		
		return result;
	}
	
	public static int shingleToInt(String text) {
		int result = 0;
		
		for (int i = 0; i < text.length(); ++i) {
			result += text.charAt(i) - 'a';
			result *= 26;
		}
		
		return result;		
	}
	
	public int getPartialSignature(int permutation, Set<String> shingles) {
		int seed = conf.getInt("similarity.seed.nr" + Integer.toString(permutation), 0);
		
		int hash = -1;
		
		for (String shingle : shingles)
			if (hash == -1 || hash > (shingleToInt(shingle) ^ seed))
				hash = shingleToInt(shingle) ^ seed;
		
		return hash;
	}
	
	public ArrayList<Integer> getSignature(Set<String> shingles) {
		ArrayList<Integer> result = new ArrayList<>();
		
		for (int i = 0; i < PERMUTATIONS; ++i)
			result.add(getPartialSignature(i, shingles));
		
		return result;
	}
	
	@Override
	public void setup(Context context) throws IOException,
	    InterruptedException {
		conf = context.getConfiguration();
	}
	
	@Override
	public void map(Object key, Text value, Context context)
			throws IOException, InterruptedException {
		if (value.toString().equals("\"Id\",\"content\""))
			return;
		
		String line = stripSpecialChars(value);
		Set<String> shingles = getShingles(line);
		ArrayList<Integer> signature = getSignature(shingles);
		
		for (int i = 0; i < BANDS; ++i) {
			String newKey = Integer.toString(i);
			for (int j = i * BAND_SIZE; j < (i + 1) * BAND_SIZE; ++j)
				newKey += ";" + Integer.toString(signature.get(j));
			context.write(new Text(newKey), value);
		}
	}
}
