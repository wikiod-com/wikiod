---
title: "Oozie data triggered coordinator"
slug: "oozie-data-triggered-coordinator"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

A detailed explanation is given on oozie data triggered coordinator job with example. 

Coordinator runs periodically from the start time until the end time. Beginning at start time, the coordinator job checks if input data is available. When the input data becomes available, a workflow is started to process the input data which on completion produces the required output data. This process is repeated at every tick of frequency until the end time of coordinator.


        <done-flag>_SUCCESS</done_flag> 
The above snippet in coordinator.xml for input dataset signals the presence of input data. That means coordinator action will be in WAITING state till _SUCCESS file is present in the given input directory. Once it is present, workflow will start execution. 

## oozie coordinator sample
The below coordinator job will trigger coordinator action once in a day that executes a workflow. The workflow has a shell script that moves input to output. 

   <?xml version="1.0" encoding="UTF-8"?>
    <coordinator-app name="log_process_coordinator" frequency="${coord:days(1)}" start="2017-04-29T06:00Z" end="2018-04-29T23:25Z" timezone="UTC"
                 xmlns="uri:oozie:coordinator:0.2">
    <datasets>
        <dataset name="input_dataset" frequency="${coord:days(1)}" initial-instance="2017-04-29T06:00Z" timezone="GMT">
            <uri-template>${nameNode}/mypath/coord_job_example/input/${YEAR}${MONTH}${DAY}</uri-template>
            <done-flag>_SUCCESS</done-flag>
        </dataset>
        <dataset name="output_dataset" frequency="${coord:days(1)}" initial-instance="2017-04-29T06:00Z" timezone="GMT">
            <uri-template>${nameNode}/mypath/coord_job_example/output/${YEAR}${MONTH}${DAY}</uri-template>
            <done-flag>_SUCCESS</done-flag>
        </dataset>
    </datasets>
    <input-events>
        <data-in name="input_event" dataset="input_dataset">
            <instance>${coord:current(0)}</instance>
        </data-in>
    </input-events>                
     <output-events>
        <data-out name="output_event" dataset="output_dataset">
            <instance>${coord:current(0)}</instance>
        </data-out>
    </output-events>
     <action>
        <workflow>
            <app-path>${workflowAppUri}</app-path>
            <configuration>
                <property>
                    <name>jobTracker</name>
                    <value>${jobTracker}</value>
                </property>
                <property>
                    <name>nameNode</name>
                    <value>${nameNode}</value>
                </property>
                <property>
                    <name>pool.name</name>
                    <value>${poolName}</value>
                </property>
                <property>
                    <name>inputDir</name>
                    <value>${coord:dataIn('input_event')}</value>
                </property>
                 <property>
                    <name>outputDir</name>
                    <value>${coord:dataOut('output_event')}</value>
                </property>
            </configuration>
        </workflow>
    </action>
</coordinator-app>

## oozie workflow sample
    <workflow-app xmlns="uri:oozie:workflow:0.4" name="shell-wf">
    <start to="shell-node"/>
    <action name="shell-node">
     <shell xmlns="uri:oozie:shell-action:0.2">
    <job-tracker>${jobTracker}</job-tracker>
    <name-node>${nameNode}</name-node>
    <configuration>
     <property>
       <name>mapred.job.queue.name</name>
       <value>${poolName}</value>
     </property>
    </configuration>
    <exec>${myscript}</exec>
    <argument>${inputDir}</argument>
    <argument>${outputDir}</argument>
    <file>${myscriptPath}</file>
    <capture-output/>
    </shell>
    <ok to="end"/>
    <error to="fail"/>
    </action>
     <kill name="fail">
      <message>Shell action failed, error message[${wf:errorMessage(wf:lastErrorNode())}]
      </message>
    </kill>
     <end name="end"/>
    </workflow-app>

## job.properties sample
    nameNode=hdfs://namenode:port
    start=2016-04-12T06:00Z
    end=2017-02-26T23:25Z
    jobTracker=yourjobtracker
    poolName=yourpool
    oozie.coord.application.path=${nameNode}/hdfs_path/coord_job_example/coord
    workflowAppUri=${oozie.coord.application.path}
    myscript=myscript.sh
    myscriptPath=${oozie.coord.application.path}/myscript.sh

## shell script sample
    inputDir=${1}
    outputDir=${2}
    hadoop fs -mkdir -p ${outputDir}
    hadoop fs -cp ${inputDir}/* ${outputDir}/


## submitting the coordinator job
Copy the script, coordinator.xml and workflow.xml into HDFS. coordinator.xml must be present in the directory specified by oozie.coord.application.path in job.properties. workflow.xml should be present in the directory specified by workflowAppUri. Once everything is in place, run the below command from shell

    oozie job -oozie <oozie_url>/oozie/ -config job.properties

