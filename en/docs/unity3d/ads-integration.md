---
title: "Ads integration"
slug: "ads-integration"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

This topic is about the integration of third-party advertisement services, such as Unity Ads or Google AdMob, into a Unity project.

This applies to [Unity Ads][1].

>**Make sure that Test Mode for Unity Ads is enabled during development**

**You, as the developer, are not allowed to generate impressions or installs by clicking on ads in your own game. Doing so violates the [Unity Ads Terms of Service][2] agreement, and you will be banned from the Unity Ads network for attempted fraud.**

For more information, read the [Unity Ads Terms of Service][2] agreement.


  [1]: https://ads.unity3d.com
  [2]: https://unity3d.com/legal/ads-publishers-terms-of-service

## Unity Ads Basics in C#
    using UnityEngine;
    using UnityEngine.Advertisements;
    
    public class Example : MonoBehaviour
    {
        #if !UNITY_ADS // If the Ads service is not enabled
        public string gameId; // Set this value from the inspector
        public bool enableTestMode = true; // Enable this during development
        #endif
    
        void InitializeAds () // Example of how to initialize the Unity Ads service
        {
            #if !UNITY_ADS // If the Ads service is not enabled
            if (Advertisement.isSupported) { // If runtime platform is supported
                Advertisement.Initialize(gameId, enableTestMode); // Initialize
            }
            #endif
        }
        
        void ShowAd () // Example of how to show an ad
        {
            if (Advertisement.isInitialized || Advertisement.IsReady()) { // If the ads are ready to be shown
                Advertisement.Show(); // Show the default ad placement
            }
        }
    }

## Unity Ads Basics in JavaScript
    #pragma strict
    import UnityEngine.Advertisements;
    
    #if !UNITY_ADS // If the Ads service is not enabled
    public var gameId : String; // Set this value from the inspector
    public var enableTestMode : boolean = true; // Enable this during development
    #endif
    
    function InitializeAds () // Example of how to initialize the Unity Ads service
    {
        #if !UNITY_ADS // If the Ads service is not enabled
        if (Advertisement.isSupported) { // If runtime platform is supported
            Advertisement.Initialize(gameId, enableTestMode); // Initialize
        }
        #endif
    }
    
    function ShowAd () // Example of how to show an ad
    {
        if (Advertisement.isInitialized && Advertisement.IsReady()) { // If the ads are ready to be shown
            Advertisement.Show(); // Show the default ad placement
        }
    }

