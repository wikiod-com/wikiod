---
title: "How to download latest artifact from Artifactory using Powershell script (v2.0 or below)?"
slug: "how-to-download-latest-artifact-from-artifactory-using-powershell-script-v20-or-below"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

This documentation explains and provides steps to download latest artifact from a JFrog Artifactory repository using Powershell Script (v2.0 or below).


## Powershell Script for downloading the latest artifcat
    $username = 'user'
    $password= 'password'
    $DESTINATION = "D:\test\latest.tar.gz"
    $client = New-Object System.Net.WebClient
    $client.Credentials = new-object System.Net.NetworkCredential($username, $password)
    $lastModifiedResponse = $client.DownloadString('https://domain.org.com/artifactory/api/storage/FOLDER/repo/?lastModified')
    [System.Reflection.Assembly]::LoadWithPartialName("System.Web.Extensions")
    $serializer = New-Object System.Web.Script.Serialization.JavaScriptSerializer
    $getLatestModifiedResponse = $serializer.DeserializeObject($lastModifiedResponse) 
    $downloaUriResponse = $getLatestModifiedResponse.uri
    Write-Host $json.uri
    $latestArtifcatUrlResponse=$client.DownloadString($downloaUriResponse)
    [System.Reflection.Assembly]::LoadWithPartialName("System.Web.Extensions")
    $serializer = New-Object System.Web.Script.Serialization.JavaScriptSerializer
    $getLatestArtifact = $serializer.DeserializeObject($latestArtifcatUrlResponse) 
    Write-Host $getLatestArtifact.downloadUri
    $SOURCE=$getLatestArtifact.downloadUri
    $client.DownloadFile($SOURCE,$DESTINATION)



