---
title: "Google Analytics in Cordova"
slug: "google-analytics-in-cordova"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Google Analytics in cordova without any plugin.

Insert the analytics function within index.js

        function analytics(){

            (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
            m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
            })(window,document,'script','lib/analytics.js','ga');
        
            if(window.localStorage) {
        
                ga('create', 'UA-XXXXXXX-1', {
                  'storage': 'none'
                  , 'clientId': window.localStorage.getItem('ga_clientId')  /*The tracker id obtained from local storage*/
                });
                ga(function(tracker) {
                  window.localStorage.setItem('ga_clientId', tracker.get('clientId'));
                 /*The tracker id for each device is different and stored in local storage*/
                });
            }
            else {
        
                ga('create', 'UA-XXXXXXX-1', 'auto');
            }

        }

        
Insert each below script tags in each html page and modify the page name
        
        <script>
            (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
            m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
            })(window,document,'script','lib/analytics.js','ga');
            
            ga('set','checkProtocolTask',null); 
            /*checkProtocal Task is set to null so that GA allows tracking other than http/https */
            
            ga('set', 'page', "Page Name");
            /*Page Name is name of each html page*/
            
            ga('send', 'pageview');
        </script>

