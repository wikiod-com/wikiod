---
title: "Nightwatch - Configuration & Setup"
slug: "nightwatch---configuration--setup"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Nightwatch has been providing Acceptance and End-to-End testing for Meteor apps since v0.5 days, and has managed migrations from PHP to Spark to Blaze and to React; and all major Continuous Integration platforms.  For additional help, please see:  

[Nightwatch API Documentation](http://nightwatchjs.org/)  
[Nightwatch.js Google Group](https://groups.google.com/forum/#!forum/nightwatchjs)  

## Configuration
The main reason that Nightwatch is so powerful, is because of it's excellent configuration file.  Unlike most other testing frameworks, Nightwatch is fully configurable and customizable to different environments and technology stacks.  



**.meteor/nightwatch.json**  

The following configuration file is for Meteor v1.3 and later, and supports two environments... a `default` environment which launches the chromedriver browser, and a `phantom` environment which runs the tests in a headless environment.  

<!-- language: lang-json -->
```
{
  "nightwatch": {
    "version": "0.9.8"
  },
  "src_folders": [
    "./tests/nightwatch/walkthroughs"
  ],
  "custom_commands_path": [
    "./tests/nightwatch/commands"
  ],
  "custom_assertions_path": [
    "./tests/nightwatch/assertions"
  ],
  "output_folder": "./tests/nightwatch/reports",
  "page_objects_path": "./tests/nightwatch/pages",
  "globals_path": "./tests/nightwatch/globals.json",
  "selenium": {
    "start_process": true,
    "server_path": "./node_modules/starrynight/node_modules/selenium-server-standalone-jar/jar/selenium-server-standalone-2.45.0.jar",
    "log_path": "tests/nightwatch/logs",
    "host": "127.0.0.1",
    "port": 4444,
    "cli_args": {
      "webdriver.chrome.driver": "./node_modules/starrynight/node_modules/chromedriver/bin/chromedriver"
    }
  },
  "test_settings": {
    "default": {
      "launch_url": "http://localhost:5000",
      "selenium_host": "127.0.0.1",
      "selenium_port": 4444,
      "pathname": "/wd/hub",
      "silent": true,
      "disable_colors": false,
      "firefox_profile": false,
      "ie_driver": "",
      "screenshots": {
        "enabled": false,
        "path": "./tests/nightwatch/screenshots"
      },
      "desiredCapabilities": {
        "browserName": "chrome",
        "javascriptEnabled": true,
        "acceptSslCerts": true,
        "loggingPrefs": {
          "browser": "ALL"
        }
      },
      "exclude": "./tests/nightwatch/unittests/*",
      "persist_globals": true,
      "detailed_output": false
    },
    "phantom": {
      "desiredCapabilities": {
        "browserName": "phantomjs",
        "javascriptEnabled": true,
        "databaseEnabled": false,
        "locationContextEnabled": false,
        "applicationCacheEnabled": false,
        "browserConnectionEnabled": false,
        "webStorageEnabled": false,
        "acceptSslCerts": true,
        "rotatable": false,
        "nativeEvents": false,
        "phantomjs.binary.path": "./node_modules/starrynight/node_modules/phantomjs-prebuilt/bin/phantomjs"
      }
    },
    "unittests": {
      "selenium": {
        "start_process": false,
        "start_session": false
      },
      "filter": "./tests/nightwatch/unittests/*",
      "exclude": ""
    }
  }
}
```

## Installation & Usage
To get Nightwatch working, you'll need a local copy of **selenium** which is a command-and-control server which manages automated browser instances.  You'll also need a web browser which selenium can control, such as **chromedriver** or **phantomjs**.  

Add the following devDependencies to your `package.json`:

<!-- language: lang-json -->
```
{
  "devDependencies": {
    "nightwatch": "0.9.8",
    "selenium-server-standalone-jar": "2.45.0",
    "chromedriver": "2.19.0",
    "phantomjs-prebuilt": "2.1.12"
  }
}
```

Then install all the depndencies.  
<!-- language: lang-json -->
```
cd myapp
meteor npm install
```


You should then be able to run Nightwatch with the following commands:

<!-- language: lang-sh -->
```
nightwatch -c .meteor/nightwatch.json
nightwatch -c .meteor/nightwatch.json --env phantom
```

If you haven't written any tests, or set up your folder structure yet, you may get some errors.  

## Setting up launch scripts
In the root of your application should be a ``package.json`` file, where you can define scripts and devDependencies.  

<!-- language: lang-json -->
```
{
  "name": "myapp",
  "version": "1.0.0",
  "scripts": {
    "start": "meteor --settings settings-development.json",
    "nightwatch": "nightwatch -c .meteor/nightwatch.json",
    "phantom": "nightwatch -c .meteor/nightwatch.json --env phantom",
  }
}
```

You will then be able to launch nightwatch with the following commands:

```
meteor npm run-script nightwatch
meteor npm run-script phantom
```

In this example, it would almost be easier to simply run `nightwatch -c .meteor/nightwatch.json`.  However, with more complex commands, with complex environment variables, options, and settings, this becomes a very useful way to specify devops scripts for a team.   


## Folder Structure
A basic Nightwatch installation for Meteor will have the following directories and files installed.  
```
/myapp
/myapp/.meteor/nightwatch.json
/client/main.html
/client/main.js
/client/main.css
/tests
/tests/nightwatch
/tests/nightwatch/assertions
/tests/nightwatch/commands
/tests/nightwatch/data
/tests/nightwatch/logs
/tests/nightwatch/pages
/tests/nightwatch/reports
/tests/nightwatch/screenshots
/tests/nightwatch/walkthroughs
/tests/nightwatch/walkthroughs/critical_path.js
/tests/nightwatch/globals.json
```

## Data Driven Testing
Nightwatch accepts a second `globals.json` configuration file which injects data into the test runner itself, very similar to how `Meteor.settings` makes data from the command line available throughout the app.  

**globals.json**  

<!-- language: lang-json -->
```
{
  "default" : {
    "url" : "http://localhost:3000",
    "user": {
      "name": "Jane Doe",
      "username" : "janedoe",
      "password" : "janedoe123",
      "email" : "janedoe@test.org",
      "userId": null
    }
  },
  "circle" : {
    "url" : "http://localhost:3000",
    "user": {
      "name": "Jane Doe",
      "username" : "janedoe",
      "password" : "janedoe123",
      "email" : "janedoe@test.org"
      "userId": null
    }
  },
  "galaxy" : {
    "url" : "http://myapp.meteorapp.com",
    "user": {
      "name": "Jane Doe",
      "username" : "janedoe",
      "password" : "janedoe123",
      "email" : "janedoe@test.org"
      "userId": null
    }
  }
}
```

You can then write your tests that aren't hardcoded with specific users, passwords, search inputs, etc.

<!-- language: lang-js -->
```
module.exports = {
  "Login App" : function (client) {
    client
      .url(client.globals.url)
      .login(client.globals.user.email, client.globals.user.password)
      .end();
  }
};
```

