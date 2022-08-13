---
title: "Mobile Analytics for Bluemix"
slug: "mobile-analytics-for-bluemix"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Mobile Analytics service on IBM Bluemix allows you to analyze the usage of your Mobile applications. Instrument your app with the Mobile Analytics SDK and gain insights on your users, the way your app behaves and the configure alerts on specific events

## Getting Started with iOS Swift apps and Mobile Analytics for Bluemix
1. Create an instance of [Mobile Analytics for Bluemix][1].
2. [Add the Bluemix Mobile Services SDK][2] to your iOS project.
3.  After installing the SDK, add these import statements at top of your AppDelegate.swift file:

<pre><code>import BMSCore
import BMSAnalytics</code></pre>

4.  Next you'll need to initialize and send mobile analytics in your didFinishLaunchingWithOptions method:

        func application(application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject: AnyObject]?) -> Bool {
            
            // Set api key, Bluemix region.  These are available in the Mobile Analytics console after you create an instance
            let api_key="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
            let bmixRegion=BMSClient.Region.usSouth

            //Provide a name for your app that will appear in the Mobile Analytics console.
            let appName="MyAppName"
        
            //Initialize Mobile Analytics in your Bluemix region
            BMSClient.sharedInstance.initialize(bluemixRegion: bmixRegion)
            Analytics.initialize(appName: appName, apiKey: api_key, hasUserContext: false,
        deviceEvents: DeviceEvent.LIFECYCLE)
        
           //Send analytics
           //Analytics.send()

           //Alternately send analytics and log to your xCode console
           Analytics.send { (response: Response?, error: NSError?) in
             if response?.statusCode == 201 {
                print("Successfully sent analytics: \(response?.responseText)")
             }
             else {
                print("Failed to send analytics: \(response?.responseText). Error: \(error?.localizedDescription)")
             }
           }
        
           return true
        }

4.  Add <code>Analytics.send()</code> into your app code everywhere you want the analytics stored in the app to be sent to the Mobile Analytics for Bluemix service.
5.  Run your app.  You will see a new user and a new session in your analytics console.


  [1]: http://new-console.ng.bluemix.net/catalog/services/mobile-analytics/
  [2]: http://new-console.ng.bluemix.net/docs/services/mobileanalytics/install-client-sdk.html#installing-sdk-ios

