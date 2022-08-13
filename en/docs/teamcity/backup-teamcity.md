---
title: "Backup TeamCity"
slug: "backup-teamcity"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| --------- | ------- |
| Backup File | The file name to use for backups. |
| Add Timestamp suffix | True or False, define if the file will have a timestamp at the end. |
| Backup scope | Define what you want to save |
| &nbsp; Basic | Saving the database, server Settings, additional data |
| &nbsp; All except build artifacts | Saving the database, server Settings, additional data, build logs, personal builds changes. |
| &nbsp;Custom | Define what you need to save. |

## How to backup
The backup menu is in the Administration Panel.
And in the Left menu, inside the `Server Administration`, go on `Backup`.

TeamCity (as of v10) does not automatically backup, but you can get TeamCity to back itself up on a daily basis by scheduling a task to hit the REST api. Typically you would also need to schedule a second task (an hour? later) to copy the finished backup off the main TeamCity server and to a safer place. 

(Note for security reasons it is better not to run an agent on the same machine as the TeamCity server)

## Backuping using TeamCity API
First of all, Ensure that the user which will be running this call has the `Change backup settings and control backup process` privilege.

    # 
    # TC Backup Launcher
    # Script to launch a backup on the TeamCity Server
    #
    Param(
        [Parameter(Mandatory=$true)][string]$username,
        [Parameter(Mandatory=$true)][string]$password
    )
    Begin
    {
        $url = "http://teamcity:8111/httpAuth/app/rest/server/backup?includeConfigs=true&includeDatabase=true&includeBuildLogs=false&fileName=TeamCity_Backup"
        $username = <username>
        $password = <password>
        
        # Function to realize a POST Operation
        function Execute-HTTPPostCommand() {
            param(
                [string] $target = $null
            )
            $request = [System.Net.WebRequest]::Create($target)
            Write-Host "POST: " $request.RequestUri
            
            $request.PreAuthenticate = $true
            $request.Method = "POST"
            $request.ContentType = "application/xml"
            $request.Headers.Add("AUTHORIZATION", "Basic");
            $request.Accept = "*"
            $request.Credentials = New-Object System.Net.NetworkCredential($username, $password)
             
            $response = $request.GetResponse()
            $xmlout = ""
            
              if($response)
              {
                  $sr = [Io.StreamReader]($response.GetResponseStream())
                  $xmlout = $sr.ReadToEnd()
              }
            return $xmlout;
        }
        
        Write-Host "Creating a new Backup:"
        
        Execute-HTTPPostCommand $url
    }

The important part is the URL to call with method POST : 

> http://teamcity:8111/httpAuth/app/rest/server/backup?includeConfigs=true&includeDatabase=true&includeBuildLogs=false&fileName=TeamCity_Backup

You can specify the settings you want to backup as in the web interface.


