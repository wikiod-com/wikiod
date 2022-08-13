---
title: "Downloading Latest Artifcat from Artifactory using Shell Script"
slug: "downloading-latest-artifcat-from-artifactory-using-shell-script"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Download latest Artifact from Artifactory repository using shell script.

## STEPS TO DOWNLOAD THE LATEST ARTIFACT
**1. Fetching the JSON response for last modified (latest) artifact**

    latestArtifactUriResponse=curl -u username:password --silent https://hostname.com/artifactory/api/storage/<repo_name>/<folder_name>/?lastModified | grep uri | awk '{ print $3 }' | sed s/\"//g | sed s/,//g

It will return a response in the following format:

    {
      "uri" : "https://hostname.com/artifactory/api/storage/<repo_name>/<folder_name>/latest_artifact.tar.gz",
      "lastModified" : "2016-12-22T04:26:25.534-0500"
    }

**2. Fetching the direct URL to the latest artifact from the "latestArtifactUriResponse"**

The `latestArtifactUriReponse` will return the response in the following format: 

    {
      "repo" : "repo_name",
      "path" : "/folder_name/latest_artifact.tar.gz",
      "created" : "2016-12-22T04:26:29.482-05:00",
      "createdBy" : "username",
      "lastModified" : "2016-12-22T04:26:25.534-05:00",
      "modifiedBy" : "username",
      "lastUpdated" : "2016-12-22T04:26:25.534-05:00",
      "downloadUri" : "https://hostname.com/artifactory/repo_name/folder_name/latest_artifact.tar.gz",
      "mimeType" : "application/octet-stream",
      "size" : "94310686",
      "checksums" : {
        "sha1" : "ocb778e566890b0f3d115b828ce8dd4e840",
        "md5" : "d050fb8108745973cf0d64e15667b340"
      },
      "originalChecksums" : {
      },
      "uri" : "https://hostanme.com/artifactory/api/storage/repo_name/folder_name/latest_artifact.tar.gz"
    }    

Here we have to fetch the value of `downloadUri` which is the direct URL to the latest artifcat and store it in `downloadUrl` variable:

    downloadUrl=`curl -u username:password --silent $latestArtifactUrl | grep downloadUri | awk '{ print $3 }' | sed s/\"//g | sed s/,//g`    

**3. Downloading the latest artifact**

`curl -u username:password -O $downloadUrl`

And you are done. 

