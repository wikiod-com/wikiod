---
title: "Working with Filesystem"
slug: "working-with-filesystem"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## How to share data across multiple devices in Win10 UWP App
To make an app more cohesive, we often need to keep user's personal settings and preferences consistent across multiple devices that have been logged in with one Microsoft account. In this sample, we use roaming data to store and to load UI settings, game process and user info. But the roaming data has its own limit: we cannot store large file in the roaming folder. The system suspends data replication for all apps in the package to the cloud until the current size no longer exceeds the maximum size. Therefore, in this sample, we haven't stored the user image in the roaming folder. Instead, it is stored in the local folder.

    private async void LoadRoamingData() 
    { 
        //Get background color 
        object color = roamingSettings.Values["BackgroundColor"]; 
        if (color != null) 
        { 
            if (ViewModel.ColorList.Keys.Contains(color.ToString())) 
            { 
                Color backgroundColor = ViewModel.ColorList[color.ToString()]; 
                ViewModel.BackgroundColor = new SolidColorBrush(backgroundColor); 
                comboBackgroundColor.SelectedValue = color.ToString(); 
            } 
        } 
        //Get game process stored in the roaming file 
        try 
        { 
            StorageFile processFile = await roamingFolder.GetFileAsync(processFileName); 
            string process = await FileIO.ReadTextAsync(processFile); 
            int gameProcess; 
            if (process != null && int.TryParse(process.ToString(), out gameProcess) && gameProcess > 0) 
            { 
                ViewModel.GameProcess = gameProcess; 
            } 
        } 
        catch { } 
 
        //Get user name 
        object userName = roamingSettings.Values["UserName"]; 
        if (userName != null && !string.IsNullOrWhiteSpace(userName.ToString())) 
        { 
            ViewModel.UserName = userName.ToString(); 
        } 
    } 

For more information, see <https://code.msdn.microsoft.com/How-to-share-data-across-d492cc0b>.

