---
title: "Install Jenkins on Windows with SSH support for private GitHub repositories"
slug: "install-jenkins-on-windows-with-ssh-support-for-private-github-repositories"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## GitHub pull requests fail
Out of the box installations of Jenkins with the Git and SSH plugins will not work when attempting to pull a private repository from GitHub.
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/COpK2.png

## PSExec.exe PS Tool by Microsoft
The first step to fix this issue I found was to download [PSTools][1] and extract the tools to a convenient location on the build server (e.g. c:\Programs\PSTools is there I extracted mine).


  [1]: https://technet.microsoft.com/en-us/sysinternals/pstools.aspx

## Generate a new SSH key just for Jenkins using PSExec or PSExec64
 1. First open the Command prompt and "Run as Administrator". 
 2. Once the command prompt is open navigate to the PSTools directory.
 3. From the command prompt we need to run git-bash using PSExec or PSExec64 as the Local Service, which Jenkins is running on the build server as by default.
 4. We will use the -i switch to run PSExec as interactive and the -s switch to run git-bash as the local service
 5. Follow the instructions for creating a ssh key on GitHub - [Generating a new SSH key and adding it to the ssh-agent][1]
 6. If you are on a 64bit Windows system, then copy the .ssh folder to C:\Windows\SysWOW64\config\systemprofile\.ssh (this was not needed on my 64bit Windows system, but there where some instructions that indicated the .ssh files should be stored there, something to keep in mind if you are having problems still).
 7. Add the public SSH key to your github keys.


    Your Commandline should look similar to this:

    C:\Programs\PSTools> PSExec.exe -i -s C:\Programs\Git\git-bash




  [1]: https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/

## Create the Jenkins Credentials
The hard part is over!  Now just create the credentials to be used in Jenkins.  Use your own Username and the passphrase used to create the SSH Key.

[![Create Jenkins Credentials to use SSH][1]][1]

This is what it should look like now (with your own private github repo and user name:
[![Successful][2]][2]


  [1]: https://i.stack.imgur.com/PjxTL.png
  [2]: https://i.stack.imgur.com/8HyPV.png

## Run a test pull request to verify, and your done.
Save and run a test pull request and your should no longer have any further problems with having Jenkins use SSH on your Windows build machine.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/dlu94.png

