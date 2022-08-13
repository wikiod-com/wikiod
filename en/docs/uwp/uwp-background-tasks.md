---
title: "UWP background tasks"
slug: "uwp-background-tasks"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

- For registering a background task that runs in a seperate process, you have to go to the "Declarations" Tab in the Package.appxmanifest and add a new "Background Task" and set the entry point.
- Registering a single-process background task can be done by means of `BackgroundTaskBuilder`, but the application will throw an exception if you register a task twice, so you must check if you have already registered a task.
- The app must gain authority to register a new task, this can be done by calling `BackgroundExecutionManager.RequestAccessAsync()`, but make sure that you really have the permission. The call returns the type of access (`BackgroundAccessStatus` enum) which will indicate whether you have access or not.
- Tasks registered are kept until the package is uninstalled, but it won't hurt to check the tasks you need on every launch, bug happens!
- When the application is updated, permission to register a new task is revoked. To keep your app running after an update, especially if you have added a new task register, you have to *remove and request the access over*, by means of `BackgroundAccessManager`. One method to know if your app is updated, is to register another task with a `SystemTrigger`, type of `SystemTriggerType.ServicingComplete`.

## Get a registered task by its name
<!-- language: c# -->   
 
    /// <summary>
    /// Gets a BackgroundTask by its name
    /// </summary>
    /// <param name="taskName">Name of the task to find</param>
    /// <returns>The found Task or null if none found</returns>
    public BackgroundTaskRegistration TaskByName(string taskName) =>
           BackgroundTaskRegistration.AllTasks.FirstOrDefault(x => x.Value.Name.Equals(taskName)).Value as BackgroundTaskRegistration;


## Unregistering a task
<!-- language: c# -->     

    /// <summary>
    /// Unregister a single background task with given name
    /// </summary>
    /// <param name="taskName">task name</param>
    /// <param name="cancelTask">true if task should be cancelled, false if allowed to finish</param>
    public void UnregisterTask(string taskName, bool cancelTask) =>
        BackgroundTaskRegistration.AllTasks.First(x => x.Value.Name.Equals(taskName)).Value?.Unregister(cancelTask);

    /// <summary>
    /// Unregister an active group of background tasks, which name contains given string
    /// </summary>
    /// <param name="taskNamePart">part of the task name</param>
    /// <param name="cancelTask">true if tasks should be cancelled, false if allowed to finish</param>
    public void UnregisterTasks(string taskNamePart, bool cancelTask)
    {
        foreach (var task in BackgroundTaskRegistration.AllTasks.Where(x => x.Value.Name.Contains(taskNamePart)))
            task.Value.Unregister(cancelTask);
    }


## Register background task with trigger
The background task are a great way to perform some work while your application is not running. Before being able to use then , you will have to register them.

Here is a sample of a background task class including the registration with a trigger and a condition and the Run implementation

    public sealed class Agent : IBackgroundTask
    {    
        public void Run(IBackgroundTaskInstance taskInstance)
        {
            // run the background task code
        }
        
        // call it when your application will start.
        // it will register the task if not already done
        private static IBackgroundTaskRegistration Register()
        {
            // get the entry point of the task. I'm reusing this as the task name in order to get an unique name
            var taskEntryPoint   = typeof(Agent).FullName;
            var taskName         = taskEntryPoint;
            
            // if the task is already registered, there is no need to register it again
            var registration            = BackgroundTaskRegistration.AllTasks.Select(x => x.Value).FirstOrDefault(x => x.Name == taskName);
            if(registration != null) return registration;
                            
            // register the task to run every 30 minutes if an internet connection is available
            var taskBuilder             = new BackgroundTaskBuilder();
            taskBuilder.Name            = taskName;
            taskBuilder.TaskEntryPoint  = taskEntryPoint;
            taskBuilder.SetTrigger(new TimeTrigger(30, false));
            taskBuilder.AddCondition(new SystemCondition(SystemConditionType.InternetAvailable));
            
            return taskBuilder.Register();
        }
    }

## Registering a Task
    /// <summary>
    /// Registers a background task in the system waiting to trigger
    /// </summary>
    /// <param name="taskName">Name of the task. Has to be unique</param>
    /// <param name="taskEntryPoint">Entry point (Namespace) of the class (has to implement IBackgroundTask and has to be in a Windows Runtime Component) to start</param>
    /// <param name="trigger">What has to be triggered to start the task</param>
    /// <param name="condition">Optional condition. Can be null</param>
    /// <param name="recreateIfExists">Should the Task be recreated if it already exists?</param>
    /// <returns></returns>
    public BackgroundTaskRegistration RegisterTask(string taskName, string taskEntryPoint, IBackgroundTrigger trigger, IBackgroundCondition condition = null) {
        Debug.WriteLine("Try registering task: " + taskName);

        var builder = new BackgroundTaskBuilder {
            Name = taskName,
            TaskEntryPoint = taskEntryPoint
        };

        builder.SetTrigger(trigger);

        if (condition != null) {
            builder.AddCondition(condition);
        }

        try {
            var task = builder.Register();
            Debug.WriteLine("Task successfully registered");
            return task;
        } catch (Exception exception) {
            Debug.WriteLine("Error creating Task: " + exception);
            return null;
        }
    }

## The task
    public sealed class BackgroundTask : IBackgroundTask {

        private BackgroundTaskDeferral _deferral;

        /// <summary>
        /// Registers the listener to check if the button is pressed
        /// </summary>
        /// <param name="taskInstance">An interface to an instance of the background task. The system creates this instance when the task has been triggered to run.</param>
        public async void Run(IBackgroundTaskInstance taskInstance) {
            _deferral = taskInstance.GetDeferral();

            //Do async operations here

            _deferral.Complete();
        }
    }

## Check if Task is registered
<!-- language: c# -->

    private bool IsTaskRegistered(string taskName) =>
            BackgroundTaskRegistration.AllTasks.Any(x => x.Value.Name.Equals(taskName));
      

## Triggering a task manually
    var trigger = new ApplicationTrigger();
    TaskHandlerMentionedInThisTutorial.RegisterTask(TaskName, entryPoint, trigger, null, true);    
    await trigger.RequestAsync();

