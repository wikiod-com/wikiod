---
title: "Cheatsheet for the Cloud Foundry command line (cf cli)"
slug: "cheatsheet-for-the-cloud-foundry-command-line-cf-cli"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

The Cloudfoundry command line interface is a wonderful thing that saves time and spares you needing to use UIs as much.  Here is my cheatsheet. First you need to install it from 
https://docs.cloudfoundry.org/cf-cli/install-go-cli.html
I have ordered the examples in order of Most Useful To Me. Your order may differ. 

 


## List apps
    cf apps
I usually start with this as now I get the AppName for cut-n-paste and if verifies I am where I want to be in space and cloud. And yes, I really really want to say space and time. 

## Restart an App
    cf rs AppName


## Tail the logs.  E.g. Do this before starting a test.
    cf logs AppName

## Dump the logs E.g. After something has gone wrong and you want to look closer.
`cf logs AppName --recent`

Note, depending on who is hosting your CF app, the size of this log may be limited.  

## Login to another region, space, organisation
CF keeps you logged in. I sometimes have to switch between regions and don't always know where I am
#Where am I?

    cf target

#Login. This is logging in to the UK region of bluemix.   Replace eu-gb with ng to go to the US.

    cf login -a https://api.eu-gb.bluemix.net


#List spaces

    cf spaces

#Change space

    cf target -s OtherSpace

#Change organisation - for when someone adds  you to their org usually. This will normally have to be followed by a target of the relevant space.

 `cf target -o OtherOrg`

## SSH to an app
    cf ssh AppName
Only for Diego apps.  Useful to see what is actually running.  

