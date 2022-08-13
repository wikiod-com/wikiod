---
title: "Communication with server"
slug: "communication-with-server"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Get
Get is getting data from web server. and `new WWW("https://urlexample.com");` with a url but without a second parameter is doing a **Get**.

i.e.

    using UnityEngine;
    using System.Collections;
    
    public class ExampleClass : MonoBehaviour 
    {
        public string url = "http://google.com";
        
        IEnumerator Start() 
        {
            WWW www = new WWW(url); // One get.
            yield return www;
            Debug.Log(www.text); // The data of the url.
        }
    }

## Simple Post (Post Fields)
Every instance of **WWW** with a second parameter is a *post*.

Here is an example to post *user id* and *password* to server.

    void Login(string id, string pwd)
    {
        WWWForm dataParameters = new WWWForm();    // Create a new form.
        dataParameters.AddField("username", id); 
        dataParameters.AddField("password", pwd);   // Add fields.
        WWW www = new WWW(url+"/account/login",dataParameters);
        StartCoroutine("PostdataEnumerator", www);
    }

    IEnumerator PostdataEnumerator(WWW www)
    {
        yield return www;
        if (!string.IsNullOrEmpty(www.error))
        {
            Debug.Log(www.error);
        }
        else
        {
            Debug.Log("Data Submitted");
        }
    }

## Post (Upload A File)
Upload a file to server is also a post. You can easily upload a file through **WWW**, like the below:

## Upload A Zip File To Server
    
    string mainUrl = "http://server/upload/";
    string saveLocation;

    void Start() 
    {
        saveLocation = "ftp:///home/xxx/x.zip"; // The file path.
        StartCoroutine(PrepareFile());
    }

    // Prepare The File.
    IEnumerator PrepareFile() 
    {
        Debug.Log("saveLoacation = " + saveLocation);
         
        // Read the zip file.
        WWW loadTheZip = new WWW(saveLocation);
 
        yield return loadTheZip;
 
        PrepareStepTwo(loadTheZip);
    }
 
    void PrepareStepTwo(WWW post) 
    {
        StartCoroutine(UploadTheZip(post));
    }

    // Upload.
    IEnumerator UploadTheZip(WWW post) 
    {
        // Create a form.
        WWWForm form = new WWWForm();
     
        // Add the file.
        form.AddBinaryData("myTestFile.zip",post.bytes,"myFile.zip","application/zip");
     
        // Send POST request.
        string url = mainUrl;
        WWW POSTZIP = new WWW(url,form);
     
        Debug.Log("Sending zip...");
        yield return POSTZIP;
        Debug.Log("Zip sent!");
    }

In this example, it use the **coroutine** to prepare and upload the file, if you want to know more about Unity coroutines, please visit [Coroutines][1].


  [1]: https://www.wikiod.com/unity3d/coroutines

## Sending a request to the server


