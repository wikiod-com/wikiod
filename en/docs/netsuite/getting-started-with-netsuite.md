---
title: "Getting started with netsuite"
slug: "getting-started-with-netsuite"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Eclipse SuiteCloud IDE Setup
1. Download and install the latest Eclipse IDE
    * Install Eclipse one of two ways:
        1. [Eclipse Installer](https://eclipse.org/downloads/)
        1. Download the [zip for your favorite package](https://eclipse.org/downloads/eclipse-packages/)
    * If you don't already have a preferred Eclipse package, *Eclipse for JavaScript Developers* is recommended
1. Install SuiteCloud IDE plugin
    1. Once installation is complete, launch Eclipse
    1. Navigate to *Help* > *Install New Software...*
    1. Click *Add...* to add a new Update Site
        * **Name**: SuiteCloud IDE
        * **Location**: http://system.netsuite.com/download/ide/update_e4
            * **Note:** the location [depends on the version of NetSuite][1] you're currently on.
            * **For example**: if you're currently on Release 2017.1 then you should use this url instead: http://system.netsuite.com/download/ide/update_17_1
    1. Select "SuiteCloud IDE" site in the *Work With* dropdown
    1. Proceed through the install wizard
    1. Restart Eclipse when prompted
1. Configure the SuiteCloud IDE plugin
    1. When Eclipse restarts, you will be prompted to set up the SuiteCloud plugin with a master password and default NetSuite account
    1. After completing this set up wizard, navigate to *Preferences* > *NetSuite*
        * Here you will find all of the SuiteCloud IDE preferences
    1. [Optional] If your primary use for Eclipse is NetSuite development, navigate to *Preferences* > *General* > *Perspectives* and make the "NetSuite" Perspective your default
1. Create a new NetSuite project
    1. Right-click in the *NS Explorer* window and select *New* > *NetSuite project*
    1. Follow the wizard for the project setup of your choosing. The project types are as follows:
        1. *Account Customization*: A project that leverages the *SuiteCloud Development Framework* for building custom objects, records, and scripts for customizing a NetSuite account.
        1. *SuiteScript*: A project used exclusively for writing scripts.
        1. *SSP Application*: A SuiteScript Server Pages application, used typically in conjunction with SiteBuilder or SuiteCommerce for NetSuite-backed E-Commerce applications.


  [1]: https://netsuite.custhelp.com/app/answers/detail/a_id/50195/kw/IDE#bridgehead_4602444786

## Hello, World 2.0 Client Script
<!-- language-all: lang-js -->

1. Create the source file for your new Client Script
    1. Create a new JavaScript file using your favorite editor or IDE
    1. Add the following source code to your file (original source [here](https://gitlab.com/stoicsoftware/learn-suitescript/blob/master/hello-world/hello-world2.js))

        ```
        define([], function () {
            /**
             * A simple "Hello, World!" example of a Client Script. Uses the `pageInit`
             * event to write a message to the console log.
             *
             * @NApiVersion 2.x
             * @NModuleScope Public
             * @NScriptType ClientScript
             */
            var exports = {};
            function pageInit(context) {
                console.log("Hello, World from a 2.0 Client Script!");
            }
            exports.pageInit = pageInit;
            return exports;
        });
        ```

    1. Save the file as `hello-world2.js` wherever you wish
1. Use the source file we just created to create a new *Script* record in NetSuite
    1. In your NetSuite account, navigate to *Customization* > *Scripting* > *Scripts* > *New*
    1. When prompted, select `hello-world2.js` as the *Script File*
    1. Click *Create Script Record*
    1. Name your Script record *Hello World*
    1. Save your new Script record
1. Deploy your new Script to the Employee record
    1. On your newly created Script record, click *Deploy Script*
    1. In the *Applies To* field, select *Employee*
    1. Make sure the *Status* field is set to *Testing*
    1. Click *Save*
1. See your script in action!
    1. Open your browser's developer/JavaScript console (typically F12 on most browsers)
    1. Create a new Employee by navigating to *Lists* > *Employees* > *Employees* > *New*
    1. Observe your "Hello, World" message in the browser console.



## Hello, World 1.0 Client Script
<!-- language-all: lang-js -->

1. Create the source file for your new Client Script
    1. Create a new JavaScript file using your favorite editor or IDE
    1. Add the following source code to your file (original source [here](https://gitlab.com/stoicsoftware/learn-suitescript/blob/master/hello-world/hello-world.js))

        ```
        /**
         * A simple "Hello, World!" example of a Client Script. Uses the `pageInit`
         * event to write a message to the console log.
         */

        function pageInit(type) {
            console.log("Hello, World from a 1.0 Client Script!");
        }
        ```

    1. Save the file as `hello-world.js` wherever you wish
1. Use the source file we just created to create a new *Script* record in NetSuite
    1. In your NetSuite account, navigate to *Customization* > *Scripting* > *Scripts* > *New*
    1. When prompted, select `hello-world.js` as the *Script File*
    1. Click *Create Script Record*
    1. When prompted, select *Client Script* as the Script Type
    1. Name your Script record *Hello World*
    1. Map the function named `pageInit` in our source file to the *Page Init* script event by entering `pageInit` in the *Page Init Function* field
    1. Save your new Script record
1. Deploy your new Script to the Employee record
    1. On your newly created Script record, click *Deploy Script*
    1. In the *Applies To* field, select *Employee*
    1. Make sure the *Status* field is set to *Testing*
    1. Click *Save*
1. See your script in action!
    1. Open your browser's developer/JavaScript console (typically F12 on most browsers)
    1. Create a new Employee by navigating to *Lists* > *Employees* > *Employees* > *New*
    1. Observe your "Hello, World" message in the browser console.



