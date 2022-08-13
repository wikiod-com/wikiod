---
title: "Getting started with hazelcast"
slug: "getting-started-with-hazelcast"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Hazelcast runs inside a Java Virtual Machine (JVM).  It is compatible with Java versions 1.6.x, 1.7.x, and 1.8.x.  Installation and setup is as simple as downloading the zip (or tar) archive, copying the uncompressed directory to a desired installation directory, and adding the jar to your Java class path.

For example, after downloading the hazelcast zip from https://hazelcast.org/download/, using bash on Linux (Hazelcast 3.7.4 is used in this example, but the version you download may be different):

    unzip hazelcast-3.7.4.zip

Set up the CLASSPATH variable:

    export CLASSPATH=${CLASSPATH}:${PWD}:hazelcast-3.7.4/lib/hazelcast-3.7.4.jar

You should now be able to start the hazelcast server to check your installation by executing the start script.  On Linux, execute the start.sh script.  Example script and expected output:

    $ hazelcast-3.7.4/bin/start.sh
    JAVA_HOME environment variable not available.
    Path to Java : /path/to/your/java

    ... ### More output here, ending with lines similar to: ### ...

    Members [1] {
        Member [192.168.38.1]:5701 - 3456f96d-3646-459b-9199-caa6ebb3e5ee this
    }

    Jan 07, 2017 8:30:53 PM com.hazelcast.core.LifecycleService
    INFO: [192.168.XX.XX]:5701 [dev] [3.7.4] [192.168.38.1]:5701 is STARTED


Note: if your java installation is not in a standard location, you may have to set the JAVA_HOME environment variable as well.

You should now be set to download and run the code samples (available at http://download.hazelcast.com/code-samples/hazelcast-code-samples-3.7.4.zip), or to start playing around with writing your own simple Java Hazelcast client to connect to the Hazelcast server node you just started.


## Hello World!
After installation of hazelcast and adding to Java Build Path, you can write `Main.class` that starts cluster work

<!-- language: java -->

    public static void main(String[] args){
        Config config = new Config();
        // creates a new HazelcastInstance (a new node in a cluster)
        HazelcastInstance instance = Hazelcast.newHazelcastInstance(config);
        // returns the Cluster that this HazelcastInstance is part of
        Cluster cluster = instance.getCluster();
        // get all devices, that are in the cluster
        Set<Member> setMembers = cluster.getMembers();

        // get ExecutorService that works on cluster instance
        ExecutorService mService = instance.getExecutorService("exec");
        
        for (int i = 0; i < setMembers.size(); i++) {
            // send a task for each member on service of HazelcastInstance
            final Future<String> future = mService.submit(new ClusterWorkingTask());
            
            String response = null;
            try {
                // wait for response
                response = future.get();
                System.out.println(response);  // each member return: Hello World!
            } catch (InterruptedException e) {
                e.printStackTrace();
            } catch (ExecutionException e) {
                e.printStackTrace();
            }
        }
    }

create `ClusterWorkingTask.class` that can be executed on each member

<!-- language: java -->

    public class ClusterWorkingTask implements Callable<String>, Serializable {
        @Override
        public String call() throws Exception {
            // send Hello World! as result of execution
            return "Hello World!";
        }
    }

