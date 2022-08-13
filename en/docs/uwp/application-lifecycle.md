---
title: "Application Lifecycle"
slug: "application-lifecycle"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Universal Windows 10 App lifecycle consists of three different states:
1) Running - application is currentyl in use
2) Not running - application is closed and removed from the memory
3) Suspended - application state is frozen but it is still in memory
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/x7MCl.png
As you cann see in the picture above there are different events connected with moving from one state to another. In examples section I show how to handle them.

It is good to refer to two good articles on MSDN Blog:

1) https://msdn.microsoft.com/en-us/windows/uwp/launch-resume/app-lifecycle
2) https://blogs.windows.com/buildingapps/2016/04/28/the-lifecycle-of-a-uwp-app/#RqKAKkevsAPIvBUT.97

## "Running" state handling
When moving to "Running" state there is special handler connected with this event:
Open "App.xaml.cx" class and see "OnLaunched" method - this is activated when applicaiton is opened by user from "Terminaded" state:

    protected override void OnLaunched(LaunchActivatedEventArgs e)
        {
            Frame rootFrame = Window.Current.Content as Frame;

            // Do not repeat app initialization when the Window already has content,
            // just ensure that the window is active
            if (rootFrame == null)
            {
                // Create a Frame to act as the navigation context and navigate to the first page
                rootFrame = new Frame();

                rootFrame.NavigationFailed += OnNavigationFailed;

               //You can get information about previous state of the app:

                if (e.PreviousExecutionState == ApplicationExecutionState.Terminated)
                {
                    //The app was previously suspended but was then shutdown 
                    //at some point because the system needed to reclaim memory.
                }
                if (e.PreviousExecutionState == ApplicationExecutionState.ClosedByUser)
                {
                    //The user closed the app with the close gesture in tablet mode,
                    //or with Alt+F4.When the user closes the app, it is first suspended
                    //and then terminated.
                }
                if (e.PreviousExecutionState == ApplicationExecutionState.NotRunning)
                {
                    //An app could be in this state because it hasn't been launched since the last time
                    //the user rebooted or logged in. It can also be in this state if it was running 
                    //but then crashed, or because the user closed it earlier.
                }
                if (e.PreviousExecutionState == ApplicationExecutionState.Running)
                {
                    //The app was already open when the user tried to launch it again
                }
                if (e.PreviousExecutionState == ApplicationExecutionState.Suspended)
                {
                    //The user either minimized or switched away from your app
                    //and didn't return to it within a few seconds.
                }

                // Place the frame in the current Window
                Window.Current.Content = rootFrame;
            }

            //When available system resources allow, the startup performance of Windows Store
            //apps on desktop device family devices is improved by proactively launching
            //the user’s most frequently used apps in the background. A prelaunched app 
            //is put into the suspended state shortly after it is launched.Then, when the
            //user invokes the app, the app is resumed by bringing it from the suspended
            //state to the running state--which is faster than launching the app cold.
            //The user's experience is that the app simply launched very quickly.
            if (e.PrelaunchActivated == false)
            {
                if (rootFrame.Content == null)
                {
                    rootFrame.Navigate(typeof(MainPage), e.Arguments);
                }
                Window.Current.Activate();
            }
        }

## "Suspending" state handling
When moving to "Suspened" state there is special handler connected with this event: Open "App.xaml.cx" class and see "App" constructor - there is event handler:

    public App()
        {
            this.InitializeComponent();
            //Handle suspending operation with event handler:
            this.Suspending += OnSuspending;
        }
Now you can handle suspension event:

    private Dictionary<string, object> _store = new Dictionary<string, object>();
    private readonly string _saveFileName = "store.xml";
    private async void OnSuspending(object sender, SuspendingEventArgs e)
        {
            var deferral = e.SuspendingOperation.GetDeferral();
            _store.Add("timestamp", DateTime.Now);
            await SaveStateAsync();
            //TODO: Save application state and stop any background activity
            //Here you can use  await SuspensionManager.SaveAsync();
            //To read more about saving state please refer to below MSDN Blog article:
            //https://blogs.windows.com/buildingapps/2016/04/28/the-lifecycle-of-a-uwp-app/#RqKAKkevsAPIvBUT.97
            deferral.Complete();
        }

        private async Task SaveStateAsync()
        {
            var ms = new MemoryStream();
            var serializer = new DataContractSerializer(typeof(Dictionary<string, object>));
            serializer.WriteObject(ms, _store);

            var file = await ApplicationData.Current.LocalFolder.CreateFileAsync(_saveFileName, CreationCollisionOption.ReplaceExisting);

            using (var fs = await file.OpenStreamForWriteAsync())
            {
                //because we have written to the stream, set the position back to start
                ms.Seek(0, SeekOrigin.Begin);
                await ms.CopyToAsync(fs);
                await fs.FlushAsync();
            }
        }

## "Resuming" state handling
Your application can be opened by user from "Suspended" state. When doing it "OnResuming" event handler is used. In "App.xaml.cs" class:

    public App()
        {
            this.InitializeComponent();
            this.Suspending += OnSuspending;
            //Handle resuming operation:
            this.Resuming += App_Resuming;
        }

    private void App_Resuming(object sender, object e)
        {
            //Do some operation connected with app resuming for instance refresh data
        }



