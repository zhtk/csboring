package pdd1;

import java.util.Random;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;

public class MainApp {
	private static Random rand = new Random(); 
	
	public static int getPermutationSeed() {
		return rand.nextInt() & 0xffffff;
	}
	
	public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		
		if (((args.length != 2) && (args.length != 3)) ||
				(args.length == 3 && !args[2].equals("-jaccard")
				&& !args[2].equals("-hamming"))) {
			System.err.println("Usage: similarity <in> <out> [-jaccard | -hamming]");
			System.exit(2);
		}
		
		if (args.length == 3)
			conf.setBoolean("similarity.jaccard", args[2].equals("-jaccard"));
		else
			conf.setBoolean("similarity.jaccard", true);
		
		for (int i = 0; i < MapTask.PERMUTATIONS; ++i)
			conf.setInt("similarity.seed.nr" + Integer.toString(i), getPermutationSeed());
		
		Job job = Job.getInstance(conf, "similarity");
		job.setJarByClass(MainApp.class);
		
		job.setMapperClass(MapTask.class);
		job.setMapOutputKeyClass(Text.class);
		job.setMapOutputValueClass(Text.class);
		
		job.setReducerClass(Reductor.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(DoubleWritable.class);

		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1] + "_tmp"));
		
		if (!job.waitForCompletion(true))
			System.exit(1);
		
		// Eliminate duplicates
		Job job2 = Job.getInstance(new Configuration(), "distinct");
		job2.setJarByClass(MainApp.class);
		job2.setMapperClass(DistinctMapper.class);
		job2.setMapOutputKeyClass(Text.class);
		job2.setMapOutputValueClass(Text.class);
		job2.setReducerClass(DistinctReducer.class);
		
		FileInputFormat.addInputPath(job2, new Path(args[1] + "_tmp"));
		FileOutputFormat.setOutputPath(job2, new Path(args[1]));
		
		System.exit(job2.waitForCompletion(true) ? 0 : 1);
	}
}
