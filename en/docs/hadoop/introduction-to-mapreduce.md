---
title: "Introduction to MapReduce"
slug: "introduction-to-mapreduce"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Syntax
 - To run the example, the command syntax is:
   
       bin/hadoop jar hadoop-*-examples.jar wordcount [-m <#maps>] [-r <#reducers>] <in-dir> <out-dir>
 - To copy data into HDFS(from local):
   
       bin/hadoop dfs -mkdir <hdfs-dir> //not required in hadoop 0.17.2 and later
       bin/hadoop dfs -copyFromLocal <local-dir> <hdfs-dir>

Word Count program using MapReduce in Hadoop.

## Word Count Program(in Java & Python)
The word count program is like the "Hello World" program in MapReduce.

Hadoop MapReduce is a software framework for easily writing applications which process vast amounts of data (multi-terabyte data-sets) in-parallel on large clusters (thousands of nodes) of commodity hardware in a reliable, fault-tolerant manner.

A MapReduce job usually splits the input data-set into independent chunks which are processed by the map tasks in a completely parallel manner. The framework sorts the outputs of the maps, which are then input to the reduce tasks. Typically both the input and the output of the job are stored in a file-system. The framework takes care of scheduling tasks, monitoring them and re-executes the failed tasks.

**Word Count Example:**

WordCount example reads text files and counts how often words occur. The input is text files and the output is text files, each line of which contains a word and the count of how often it occured, separated by a tab.

Each mapper takes a line as input and breaks it into words. It then emits a key/value pair of the word and each reducer sums the counts for each word and emits a single key/value with the word and sum.

As an optimization, the reducer is also used as a combiner on the map outputs. This reduces the amount of data sent across the network by combining each word into a single record.

**Word Count Code:**

<!-- language: lang-java -->

    package org.myorg;
            
    import java.io.IOException;
    import java.util.*;
            
    import org.apache.hadoop.fs.Path;
    import org.apache.hadoop.conf.*;
    import org.apache.hadoop.io.*;
    import org.apache.hadoop.mapreduce.*;
    import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
    import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
    import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
    import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
            
    public class WordCount {
            
     public static class Map extends Mapper<LongWritable, Text, Text, IntWritable> {
        private final static IntWritable one = new IntWritable(1);
        private Text word = new Text();
            
        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            String line = value.toString();
            StringTokenizer tokenizer = new StringTokenizer(line);
            while (tokenizer.hasMoreTokens()) {
                word.set(tokenizer.nextToken());
                context.write(word, one);
            }
        }
     } 
            
     public static class Reduce extends Reducer<Text, IntWritable, Text, IntWritable> {
    
        public void reduce(Text key, Iterable<IntWritable> values, Context context) 
          throws IOException, InterruptedException {
            int sum = 0;
            for (IntWritable val : values) {
                sum += val.get();
            }
            context.write(key, new IntWritable(sum));
        }
     }
            
     public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
            
            Job job = new Job(conf, "wordcount");
        
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(IntWritable.class);
            
        job.setMapperClass(Map.class);
        job.setReducerClass(Reduce.class);
            
        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
            
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));
            
        job.waitForCompletion(true);
     }
            
    }


To run the example, the command syntax is:

    bin/hadoop jar hadoop-*-examples.jar wordcount [-m <#maps>] [-r <#reducers>] <in-dir> <out-dir>

All of the files in the input directory (called in-dir in the command line above) are read and the counts of words in the input are written to the output directory (called out-dir above). It is assumed that both inputs and outputs are stored in HDFS.If your input is not already in HDFS, but is rather in a local file system somewhere, you need to copy the data into HDFS using a command like this:

    bin/hadoop dfs -mkdir <hdfs-dir> //not required in hadoop 0.17.2 and later
    bin/hadoop dfs -copyFromLocal <local-dir> <hdfs-dir>


Word Count example in Python:

mapper.py

    import sys 
    for line in sys.stdin: 
        # remove leading and trailing whitespace 
        line = line.strip() 
        # split the line into words 
        words = line.split() 
        # increase counters 
        for word in words: 
            print '%s\t%s' % (word, 1)

reducer.py

    import sys
    current_word = None
    current_count = 0
    word = None
    for line in sys.stdin:
        # remove leading and trailing whitespaces
        line = line.strip()
        # parse the input we got from mapper.py
        word, count = line.split('\t', 1)
        # convert count (currently a string) to int
        try:
            count = int(count)
        except ValueError:
            # count was not a number, so silently
            # ignore/discard this line
            continue
        if current_word == word:
            current_count += count
        else:
            if current_word:
                print '%s\t%s' % (current_word, current_count)
            current_count = count
            current_word = word
    if current_word == word:
        print '%s\t%s' % (current_word, current_count)
     
The above program can be run using ` cat filename.txt | python mapper.py | sort -k1,1 | python reducer.py`

