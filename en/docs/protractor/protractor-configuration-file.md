---
title: "Protractor configuration file"
slug: "protractor-configuration-file"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

The configuration file contains information which Protractor uses to run your test script. Here I'll try to give a few different variations. 

## Simple Config file - Chrome
    var config = {};
    var timeout = 120000;
    
    config.framework = 'jasmine2';
    config.allScriptsTimeout = timeout;
    config.getPageTimeout = timeout;
    config.jasmineNodeOpts.isVerbose = true;
    config.jasmineNodeOpts.defaultTimeoutInterval = timeout;
    config.specs = ['qa/**/*Spec.js'];
    config.browserName = 'chrome';

    exports.config = config;

## Config file with capabilities - Chrome
    var config = {};
    var timeout = 120000;
    
    config.framework = 'jasmine2';
    config.allScriptsTimeout = timeout;
    config.getPageTimeout = timeout;
    config.jasmineNodeOpts.isVerbose = true;
    config.jasmineNodeOpts.defaultTimeoutInterval = timeout;
    config.specs = ['qa/**/*Spec.js'];
    config.capabilities = {
        browserName: 'chrome',
        'chromeOptions': {
            'args': ['start-minimized', 'window-size=1920,1080']
        }
    };
    
    exports.config = config;

## config file shardTestFiles - Chrome
This configuration lets' you run your total spec files in two browser instances in parallel. It helps reduce the overall test execution time. Change the maxInstances based on your need.

**Note**: *Make sure your tests are independent.*

    var config = {};
    var timeout = 120000;
    
    config.framework = 'jasmine2';
    config.allScriptsTimeout = timeout;
    config.getPageTimeout = timeout;
    config.jasmineNodeOpts.isVerbose = true;
    config.jasmineNodeOpts.defaultTimeoutInterval = timeout;
    config.specs = ['qa/**/*Spec.js'];
    config.capabilities = {
        browserName: 'chrome',
        shardTestFiles: true,
        maxInstances: 2,
        'chromeOptions': {
            'args': ['start-minimized', 'window-size=1920,1080']
        }
    };
    
    exports.config = config;

## config file multi-capabilities emulate - chrome
    var config = {};
    var timeout = 120000;

    config.framework = 'jasmine2';
    config.allScriptsTimeout = timeout;
    config.getPageTimeout = timeout;
    config.jasmineNodeOpts.isVerbose = true;
    config.jasmineNodeOpts.defaultTimeoutInterval = timeout;
    config.specs = ['qa/**/*Spec.js'];
    config.multiCapabilities = [{
            browserName: 'chrome',
            shardTestFiles: true,
            maxInstances: 2,
            'chromeOptions': {
                'args': ['start-minimized', 'window-size=1920,1080']
            }
        },
        {
            browserName: 'chrome',
            shardTestFiles: true,
            maxInstances: 1,
            'chromeOptions': {
                'args': ['show-fps-counter=true'],
                'mobileEmulation': {
                    'deviceName': 'Apple iPhone 6'
                }
            }
        }
    ];

    exports.config = config;

