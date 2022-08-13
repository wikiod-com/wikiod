---
title: "Continuous Integration & Device Clouds (with Nightwatch)"
slug: "continuous-integration--device-clouds-with-nightwatch"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Nightwatch has been providing Acceptance and End-to-End testing for Meteor apps since v0.5 days, and has managed migrations from PHP to Spark to Blaze and to React; and all major Continuous Integration platforms.  For additional help, please see:  

[Nightwatch API Documentation](http://nightwatchjs.org/)  
[Nightwatch.js Google Group](https://groups.google.com/forum/#!forum/nightwatchjs)  

## Travis
Travis is the original Continuous Integration service that became popular in the Meteor community.  It's solid and reliable, has long had a open-source hosting tier, and has run hundreds of thousands of Nightwatch tests over the years.  

**.travis.yml**  
Simply put a `.travis.yml` file in the root of your application, like so:

<!-- language: lang-yml -->
```
# this travis.yml file is for the leaderboard-nightwatch example, when run standalone
language: node_js

node_js:
  - "0.10.38"

services:
  - mongodb

sudo: required

env:
  global:
    - TRAVIS=true
    - CONFIG_PREFIX=`npm config get prefix`
    - DISPLAY=:99.0
    - NODE_ENV=`travis`
  matrix:

cache:
  directories:
    - .meteor/local/build/programs/server/assets/packages
    - .meteor

before_install:
  # set up the node_modules dir, so we know where it is
  - "mkdir -p node_modules &"

  # install nightwatch, selenium, , so we can launch nightwatch and selenium
  - "meteor npm install nightwatch selenium-server-standalone-jar chromedriver"

  # fire up xvfb on port :99.0
  - "sh -e /etc/init.d/xvfb start"

  # set the xvfb screen size to 1280x1024x16
  - "/sbin/start-stop-daemon --start --quiet --pidfile /tmp/custom_xvfb_99.pid --make-pidfile --background --exec /usr/bin/Xvfb -- :99 -ac -screen 0 1280x1024x16"

  # install meteor
  - "curl https://install.meteor.com | /bin/sh"

  # give meteor a few seconds after installing
  - "sleep 10"

  # setup Meteor app
  - "cd webapp"
  - "meteor &"

  # give Meteor some time to download packages, init data, and to start
  - "sleep 60"

# then run nightwatch using the chromedriver
script: "nightwatch -c .meteor/nightwatch.json"
```

## Circle
[Circle](https://circleci.com/) is the newer Continuous Integration service that's become popular among Meteorites.  It's got all of the latest bells and whistles, as far as continuous integration goes.  The following script supports many new features, including:

 - screenshots
 - artifacts
 - git submodules
 - environment detection
 - directory caching
 - parallelism optimization
 - npm scripts
 - continuous deployment
 - webhooks

**.circle.yml**  
<!-- language: lang-yml -->
```
## Customize the test machine
machine:

  # Timezone
  timezone:
    America/Los_Angeles # Set the timezone

  # Add some environment variables
  environment:
    CIRCLE_ENV: test
    CXX: g++-4.8
    DISPLAY: :99.0
    NPM_PREFIX: /home/ubuntu/nvm/v0.10.33
    INITIALIZE: true
    NODE_ENV: circle


## Customize checkout
checkout:
 post:
   #- git submodule sync
   #- git submodule update --init --recursive # use submodules

general:
  build_dir: webapp
  artifacts:
    - "./tests/nightwatch/screenshots" # relative to the build directory

## Customize dependencies
dependencies:
  cache_directories:
    - "~/.meteor" # relative to the user's home directory
    - ~/nvm/v0.10.33/lib/node_modules/starrynight
    - ~/nvm/v0.10.33/bin/starrynight

  pre:
    # Install Starrynight unless it is cached
    - if [ ! -e ~/nvm/v0.10.33/bin/starrynight ]; then npm install -g starrynight; else echo "Starrynight seems to be cached"; fi;
    # Install  Meteor
    - mkdir -p ${HOME}/.meteor
    # If Meteor is already cached, do not need to build it again.
    - if [ ! -e ${HOME}/.meteor/meteor ]; then curl https://install.meteor.com | /bin/sh; else echo "Meteor seems to be cached"; fi;
    # Link the meteor executable into /usr/bin
    - sudo ln -s $HOME/.meteor/meteor /usr/bin/meteor
    # Check if the helloworld directory already exists, if it doesn't, create the helloworld app
    # The following doesn't work, because it should be checking ${HOME}/active-entry/helloworld
    # - if [ ! -e ${HOME}/helloworld ]; then meteor create --release METEOR@1.1.0.3 helloworld; else echo "helloworld app seems to be cached"; fi;

  override:
    #- meteor list

## Customize test commands
test:
  pre:
    #- starrynight fetch
    #- cd packages && rm -rf temp
    #- cd packages && ls -la
    #- starrynight autoconfig
    - meteor update --release METEOR@1.3.3
    - meteor npm install --save jquery bootstrap react react-dom react-router react-bootstrap react-komposer 
    - cat .meteor/nightwatch.json
    - meteor:
          background: true
    - sleep 60
  override:
    - meteor npm run-script nightwatch


## Customize deployment commands
#deployment:
#  production:
#    branch: master
#    commands:
#      - printf "<Meteor username>\n<Meteor password>\n" | meteor deploy myapp.meteor.com

## Custom notifications
#notify:
  #webhooks:
    # A list of hashes representing hooks. Only the url field is supported.
    #- url: https://someurl.com/hooks/circle

```




## SauceLabs
[SauceLabs](https://saucelabs.com/) is an Automated Testing Platform for the enterprise.  It supports both continuous integration, cross browser testing, and a mobile device cloud.  Costs are higher than with Travis, Circle, or BrowserStack, hwoever.

<!-- language: lang-json -->
```
{
  "selenium" : {
    "start_process" : false,
    "host" : "ondemand.saucelabs.com",
    "port" : 80,
  },
  "test_settings" : {
    "chrome_saucelabs": {
      "selenium_host": "ondemand.saucelabs.com",
      "selenium_port": 80,
      "username": "${SAUCE_USERNAME}",
      "access_key": "${SAUCE_ACCESS_KEY}",
      "use_ssl": false,
      "silent": true,
      "output": true,
      "screenshots": {
        "enabled": false,
        "on_failure": true,
        "path": ""
      },
      "desiredCapabilities": {
        "name": "test-example",
        "browserName": "chrome"
      },
      "globals": {
        "myGlobal": "some_sauce_global"
      }
    },
  }
}
```

## BrowserStack
[BrowserStack](https://www.browserstack.com/) uses a device cloud for cross-browser testing. The intent is to allow testing of Selenium scripts on every device possible.  

<!-- language: lang-json -->
```
{
  "selenium" : {
    "start_process" : false,
    "host" : "hub.browserstack.com",
    "port" : 80,
  },

  "test_settings" : {
    "default" : {
      "launch_url" : "http://hub.browserstack.com",
      "selenium_port"  : 80,
      "selenium_host"  : "hub.browserstack.com",
      "silent": true,
      "screenshots" : {
        "enabled" : false,
        "path" : "",
      },
      "desiredCapabilities": {
        "browserName": "firefox",
        "javascriptEnabled": true,
        "acceptSslCerts": true,
        "browserstack.user": "USERNAME",
        "browserstack.key": "KEY"
      }
    }
  }
}
```

