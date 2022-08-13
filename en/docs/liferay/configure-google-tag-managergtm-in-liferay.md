---
title: "Configure Google Tag manager(GTM) in liferay"
slug: "configure-google-tag-managergtm-in-liferay"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

This documentation is not specific to liferay but can be used with reference to any web application.

Liferay provides Google Analytics(referred as GA ahead) by default,after configuring Analytics id GA-##### in Site settings.But this provides limited functionality,only allowing to track page views(Page title and URL).In order to expand it further,we can either embed GA script directly onto the site theme to trigger the required events or use GTM.

## Using GTM to configure GA events
GTM simplifies the whole process of managing tags.In GTM terminology

 1. We put a GTM javascript snippet on the concerned page,in portal_normal.vm in custom theme in liferay, containing the GTM id and a data layer structure(if needed) to map values from page to variables
 2. Corresponding to data layer variables,we need to create Variables at GTM end,which retrieve data from data layer
 3. Subsequently,we create tags,which are basically fields which maps variables from data layer to events,which are triggered on certain conditions,leading to events being sent to respective tracking tool(GA,in our case).

Below is a sample of GTM javascript snippet embedded on a page,

        <body>
        <!-- 1) Data layer section -->
        <script type="text/javascript">
            dataLayer = [{
                    "page" : "<? Virtual path of the page ?>"
                    ,"pageType" : "<? Page type ?>"
                    ,"user" : {
                                "type" : "<? User type ?>"
                                ,"userId" : "<? Logged user id ?>"
                                ,"country" : "<? Logged user country ?>"
                                ,"userRole" : "<? Role of user ?>"
                            }
                }];
          </script>
        <!-- 2) GTM Container -->
        <noscript><iframe src="//www.googletagmanager.com/ns.html?id=GTM-PK9HK8"
        height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
        <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
        new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
        j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
        '//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
        })(window,document,'script','dataLayer','<GTM-ID>');</script>
        <!-- End Google Tag Manager -->
Post including this script in page,we need to configure the respective variables and tags from GTM end.

[![enter image description here][1]][1]
[![enter image description here][2]][2]
[![enter image description here][3]][3]
[![enter image description here][4]][4]

Post we have configured the required fields,we can view events on GA console upon a user view.

[![enter image description here][5]][5]

In order to view the data sent from portal to GA,we can use [Google Analytics Debugger][6] plugin,to view events being sent to GA via browser console.


  [1]: https://i.stack.imgur.com/fULpl.png
  [2]: https://i.stack.imgur.com/H765R.png
  [3]: https://i.stack.imgur.com/uefBn.png
  [4]: https://i.stack.imgur.com/sQLOb.png
  [5]: https://i.stack.imgur.com/rlHQC.png
  [6]: https://chrome.google.com/webstore/detail/google-analytics-debugger/jnkmfdileelhofjcijamephohjechhna

