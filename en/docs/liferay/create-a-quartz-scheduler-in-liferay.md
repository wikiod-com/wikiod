---
title: "Create a Quartz scheduler in liferay"
slug: "create-a-quartz-scheduler-in-liferay"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

A scheduler serves to perform background tasks at certain defined intervals.

As per [Liferay portlet DTD][1] 


<!-
The scheduler-entry element contains the declarative data of a scheduler.
->

> !ELEMENT scheduler-entry (scheduler-description?,
> scheduler-event-listener-class, trigger)

<!-
The scheduler-description value describes a scheduler.
->

    

> !ELEMENT
> scheduler-description (#PCDATA)

<!-
The scheduler-event-listener-class value must be a class that implements
com.liferay.portal.kernel.messaging.MessageListener. This class will receive
a message at a regular interval specified by the trigger element. ->

> !ELEMENT
> scheduler-event-listener-class (#PCDATA)

<!-
The trigger element contains configuration data to indicate when to trigger the
class specified in scheduler-event-listener-class.
->

> !ELEMENT trigger (cron |
> simple)


  [1]: http://www.liferay.com/dtd/liferay-portlet-app_6_2_0.dtd

## Create a quartz scheduler to display some information
In order to create a scheduler,the entry needs to be created in 

    liferay-portlet.xml
provding scheduler class and trigger value for timing of scheduler triggering

    <portlet-name>GetSetGo</portlet-name>
            <icon>/icon.png</icon>
            <scheduler-entry>
            <scheduler-description>This scheduler logs User count from portal</scheduler-description>
                <scheduler-event-listener-class>com.example.scheduler.SchedulerSample</scheduler-event-listener-class>
                <trigger>
                     <simple>
                       <simple-trigger-value>
                         5
                       </simple-trigger-value>
                    <time-unit>minute</time-unit>
                </simple>
                </trigger>
            </scheduler-entry>
The given entry provides 

 1. Scheduler description
 2. Class name,which implements MessageListener class
 3. Trigger,which provides intervals for defining trigger point for scheduler
     
       -Using Cron
       
       -Using Simple trigger value
 
In the given example,the scheduler will trigger after every 5 mins.

Next up we need to create scheduler class

    package com.example.scheduler;
    
    import com.liferay.portal.kernel.exception.SystemException;
    import com.liferay.portal.kernel.log.Log;
    import com.liferay.portal.kernel.log.LogFactoryUtil;
    import com.liferay.portal.kernel.messaging.Message;
    import com.liferay.portal.kernel.messaging.MessageListener;
    import com.liferay.portal.kernel.messaging.MessageListenerException;
    import com.liferay.portal.service.UserLocalServiceUtil;
    
    public class SchedulerSample implements MessageListener {
    
        @Override
        public void receive(Message arg0) throws MessageListenerException {
            Log log=LogFactoryUtil.getLog(SchedulerSample.class);
            
            try {
                log.info("User Count for portal:"+UserLocalServiceUtil.getUsersCount());
            } catch (SystemException e) {
                
                log.info("User count is currently unavailable");
            }
            
        }
    
    }

This scheduler simply displays output portal user count after every trigger interval to server console.

## Create a dynamic quartz scheduler programmatically
There are specific scenarios where in we might need to create a Quartz scheduler,based on user input on when a scheduler should be triggered,apart from we can handle cases,where we have certain pre-defined functionalities,which need to be triggered based on user action,at a certain period.

This example receives user input on trigger timing,to trigger a scheduler.Here `ScheduledJobListener` class `imlements MessageListener`,which contains business logic to be executed on triggering the scheduler.The job is scheduled using `SchedulerEngineHelperUtil`class to trigger the job,after configuring the required params:

 1. Trigger(using the cron text string and job name)
 2. Message(using implementation for MessageListener class and portletId)
 3. Scheduler storage types(which is MEMORY_CLUSTERED by default,can be set as PERSISTED to be stored in DB)
 4. DestinationNames(which is SCHEDULER_DISPATCH for Liferay) which decides Message Bus destination to be used

 
The below snippet is part of action phase of the portlet interacting with user,to create and schedule a quartz job.

       //Dynamic scheduling
        String portletId= (String)req.getAttribute(WebKeys.PORTLET_ID);
                      
        String jobName= ScheduledJobListener.class.getName();
               
        Calendar startCalendar = new GregorianCalendar(year , month, day, hh, mm, ss);
        String jobCronPattern = SchedulerEngineHelperUtil.getCronText(startCalendar, false);
                                    //Calendar object & flag for time zone sensitive calendar
        
        Trigger trigger=new CronTrigger(ScheduledJobListener.class.getName(),ScheduledJobListener.class.getName(), jobCronPattern);
                
        Message message=new Message();
        message.put(SchedulerEngine.MESSAGE_LISTENER_CLASS_NAME,jobName);
        message.put(SchedulerEngine.PORTLET_ID, portletId);
        
        try {
              SchedulerEngineHelperUtil.schedule(
                    trigger,StorageType.PERSISTED,"Message_Desc",DestinationNames.SCHEDULER_DISPATCH,
                    message,0);                    
             } catch (SchedulerException e) 
                    {
                        e.printStackTrace();
                    }
   Here,in order to create cron text,params are retrieved from user input
   For the cron text,we can also use the given reference for creating the cron pattern

   
        1. Seconds
        2. Minutes
        3. Hours
        4. Day-of-Month
        5. Month
        6. Day-of-Week
        7. Year (optional field)
        **Expression**     **Meaning**
        0 0 12 * * ?     Fire at 12pm (noon) every day
        0 15 10 ? * *     Fire at 10:15am every day
        0 15 10 * * ?     Fire at 10:15am every day
        0 15 10 * * ? *     Fire at 10:15am every day
        0 15 10 * * ? 2005     Fire at 10:15am every day during the year 2005
        0 * 14 * * ?     Fire every minute starting at 2pm and ending at 2:59pm, every day
        0 0/5 14 * * ?     Fire every 5 minutes starting at 2pm and ending at 2:55pm, every day
        0 0/5 14,18 * * ?     Fire every 5 minutes starting at 2pm and ending at 2:55pm, AND fire every 5 minutes starting at 6pm and ending at 6:55pm, every day
        0 0-5 14 * * ?     Fire every minute starting at 2pm and ending at 2:05pm, every day
        0 10,44 14 ? 3 WED     Fire at 2:10pm and at 2:44pm every Wednesday in the month of March.
        0 15 10 ? * MON-FRI     Fire at 10:15am every Monday, Tuesday, Wednesday, Thursday and Friday
        0 15 10 15 * ?     Fire at 10:15am on the 15th day of every month
        0 15 10 L * ?     Fire at 10:15am on the last day of every month
        0 15 10 L-2 * ?     Fire at 10:15am on the 2nd-to-last last day of every month
        0 15 10 ? * 6L     Fire at 10:15am on the last Friday of every month
        0 15 10 ? * 6L     Fire at 10:15am on the last Friday of every month
        0 15 10 ? * 6L 2002-2005     Fire at 10:15am on every last friday of every month during the years 2002, 2003, 2004 and 2005
        0 15 10 ? * 6#3     Fire at 10:15am on the third Friday of every month
        0 0 12 1/5 * ?     Fire at 12pm (noon) every 5 days every month, starting on the first day of the month.
        0 11 11 11 11 ?     Fire every November 11th at 11:11am.
   
and directly create a crontext string to be used based on user input

     String jobCronPattern="0 */5 * * * ?";
Here in this case,it fires after every five minutes.

References:

 1. [Dynamic scheduler creation][1]
 2. [Scheduler application][2]
 3. [Quartz FAQs][3]

     


  [1]: https://web.liferay.com/community/forums/-/message_boards/message/48993955
  [2]: https://web.liferay.com/marketplace/-/mp/application/35025281
  [3]: http://www.quartz-scheduler.org/documentation/faq.html

