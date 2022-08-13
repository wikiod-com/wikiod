---
title: "Getting started with behat"
slug: "getting-started-with-behat"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Functional testing as user stories
Functional tests are best described as tests for your user stories. If you've dealt with user stories before they normally follow the following pattern:

    As a [role], I want to [desire], so that [benefit/desired outcome]

For the following examples we'll use this user story as an example:

    As a Dungeon Master, I want to ensure harmony and mutual trust, so that
    the party can work together as a team

The two most popular testing frameworks for functional tests in PHP are [Behat](http://www.behat.org) and [PHPSpec](http://www.phpspec.net).

## Beginning with Behat


## Extending Behat with Mink


## Testing JavaScript with Mink and Selenium


## Setting up test data


## Capturing emails


## Installation or Setup
Behat/Mink

Install using composer (for other methods check  ) behat.org
If you using linux, please go sure that you have installed php-curl (normal curl installation won't work)

**Linux**

    sudo apt-get install php5-curl

If you are using **Windows**, make sure you have PHP, Curl and Git installed.
You can find those under following links:

 - PHP (Xampp) : https://www.apachefriends.org/de/index.html
 - Curl: http://curl.haxx.se/latest.cgi?curl=win64-nossl
 - Git: http://git-scm.com/download/win

Your composer.json would contain the following:

**behat - composer.json**

    {
      "require": {
        "behat/behat": "dev-master",
        "behat/mink": "dev-master",
        "behat/mink-extension": "dev-master",
        "behat/mink-selenium2-driver": "dev-master",
        "phpunit/php-code-coverage": "dev-master",
        "phpunit/phpunit-mock-objects": "dev-master",
        "phpunit/phpunit": "dev-master"
      },
      "minimum-stability": "dev",
      "config": {
        "bin-dir": "bin/"
      }
    }
(when saving the composer.json file in Windows, you need to choose "All files" as Filetype and "ANSI" coding)

Afterwards execute the following commands:

    $ curl http://getcomposer.org/installer | php
    $ php composer.phar install

After this Behat, Mink and Behat-Mink extension are installed,
To execute behat

**execute behat**
    
    $ bin/behat

To activate the Behat-Mink Extension use: behat.yml
create a file "behat.yml" with the following content

**behat.yml**

    default:
      suites:
        default:
          paths:
            features: %paths.base%/features/
            bootstrap: %paths.base%/features/bootstrap/
          contexts: 
            - FeatureContext
      extensions:
        Behat\MinkExtension:
          base_url: 'http://www.startTestUrl.de'
          selenium2:
            browser: firefox
            wd_host: "http://localhost:4444/wd/hub"

This file will be in the same directory that contains bin directory and link to behat.<br>
Also note that in the yml file, do not use tabs for indentation. use spaces.
To get a list of commands available in behat-mink, use

    $ bin/behat -di

**Make behat part of your system**

Linux

Go to your Homedirectory and do the following:

    $ sudo vi .bashrc

And add this lines at the end of the directory

    export BEHAT_HOME=/home/*user*/path/to/behat
    export PATH=$BEHAT_HOME/bin:$PATH

Restart the console or type "source .bashrc"

**Windows**

Go over the Systemsettings and add the Path of behat/bin to the environment-variables


**Other Drivers**
Over drivers like Selenium, phantomjs, goutte, etc. must be installed too.

