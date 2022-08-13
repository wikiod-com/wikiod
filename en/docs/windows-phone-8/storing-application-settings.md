---
title: "Storing Application Settings"
slug: "storing-application-settings"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Adding a value
    //Add value to Isolated Storage Settings    
    IsolatedStorageSettings.ApplicationSettings.Add("Key", "Value");

    

## Checking if a value exists
    //Check if a value exists in settings already
    if (IsolatedStorageSettings.ApplicationSettings.Contains("Key"))
    {
        //perform logic
    }

## Reading the value
    string setting = IsolatedStorageSettings.ApplicationSettings["Key"] as string;

## Saving the settings
    IsolatedStorageSettings.ApplicationSettings.Save();

