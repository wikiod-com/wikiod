---
title: "Naming Files when uploading with Laravel on Windows"
slug: "naming-files-when-uploading-with-laravel-on-windows"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Parameters

| Param/Function| Description
| ------ | ------ |
| file-upload| name of the file `<input>` field|
| $sampleName| could also be dynamically generated string or the name of the file uploaded by the user|  
| app_path()| is Laravel helper to provide the absolute path to the application|  
| getCLientOriginalExtension()| Laravel wrapper to fetch the extension of the file uploaded by the user as it was on the user machine|

## Generating timestamped file names for files uploaded by users.
**Below won't work on a Windows machine**  

    $file = $request->file('file_upload');
    $sampleName = 'UserUpload';
    $destination = app_path() . '/myStorage/';
    $fileName = $sampleName . '-' . date('Y-m-d-H:i:s') . '.' . 
    $file->getClientOriginalExtension();
    $file->move($destination, $fileName);  
It will throw an error like "Could no move file to /path..."  
  
**Why? - This works perfectly on a Ubuntu server**  
The reason is that on Windows `colon ':'` is not allowed in a filename where as it is allowed on linux. This is such a small thing that we may not notice it upfront and keep wondering that why a code which is running well on Ubuntu (Linux) is not working?  
Our first hunch would be to check the file permissions and things like that but we may not notice that `colon ':'` is the culprit here.  
So in order to upload files on Windows, **Do not use `colon':'` while generating timestamped filenames**, instead do something like below:  
    
    $filename = $sampleName . '-' . date('Y-m-d-H_i_s') . '.' . $file->getClientOriginalExtension();  //ex output UserUpload-2016-02-18-11_25_43.xlsx
                
                                       OR
 
    $filename = $sampleName . '-' .date('Y-m-d H i s') . '.' . $file->getClientOriginalExtension();  //ex output UserUpload-2016-02-18 11 25 43.xlsx
    
                                       OR
    
    $filename = $sampleName . '-'.date('Y-m-d_g-i-A').'.' . $file->getClientOriginalExtension(); //ex output UserUpload-2016-02-18_11-25-AM.xlsx

