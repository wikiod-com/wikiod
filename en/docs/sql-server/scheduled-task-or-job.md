---
title: "Scheduled Task or Job"
slug: "scheduled-task-or-job"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

SQL Server Agent uses SQL Server to store job information. Jobs contain one or more job steps. Each step contains its own task,i.e: backing up a database.

SQL Server Agent can run a job on a schedule, in response to a specific event, or on demand.

## Create a scheduled Job
**Create a Job**

- To add a job first we have to use a stored procedure named [sp_add_job](https://msdn.microsoft.com/en-us/library/ms187358.aspx)

      USE msdb ;  
      GO  
      EXEC dbo.sp_add_job  
      @job_name = N'Weekly Job' ;  -- the job name

- Then we have to add a job step using a stored procedure named [sp_add_jobStep](https://msdn.microsoft.com/en-us/library/ms178625.aspx)

      EXEC sp_add_jobstep  
      @job_name = N'Weekly Job',  -- Job name to add a step
      @step_name = N'Set database to read only',  -- step name
      @subsystem = N'TSQL',  -- Step type
      @command = N'ALTER DATABASE SALES SET READ_ONLY',   -- Command
      @retry_attempts = 5,  --Number of attempts
      @retry_interval = 5 ; -- in minutes

 - Target the job to a server

       EXEC dbo.sp_add_jobserver  
       @job_name = N'Weekly Sales Data Backup',
       @server_name = 'MyPC\data;   -- Default is LOCAL
       GO


**Create a schedule using SQL**

To Create a schedule we have to use a system stored procedure called [sp_add_schedule](https://msdn.microsoft.com/en-us/library/ms187320.aspx)

    USE msdb 
    GO  

    EXEC sp_add_schedule  
        @schedule_name = N'NightlyJobs' ,  -- specify the schedule name
        @freq_type = 4,   -- A value indicating when a job is to be executed (4) means Daily
        @freq_interval = 1,  -- The days that a job is executed and depends on the value of `freq_type`.
        @active_start_time = 010000 ;   -- The time on which execution of a job can begin
    GO  

There are more parameters that can be used with `sp_add_schedule` you can read more about in the the link provided above.

**Attaching schedule to a JOB**

To attach a schedule to an SQL agent job you have to use a stored procedure called [sp_attach_schedule](https://msdn.microsoft.com/en-us/library/ms186766.aspx) 

    -- attaches the schedule to the job BackupDatabase  
    EXEC sp_attach_schedule  
       @job_name = N'BackupDatabase',  -- The job name to attach with
       @schedule_name = N'NightlyJobs' ;  -- The schedule name
    GO  


<br>



    




    

    

    

    


