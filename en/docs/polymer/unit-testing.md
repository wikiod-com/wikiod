---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

[Web Component Tester](https://github.com/Polymer/web-component-tester) - the tool for unit testing apps built with Polymer. You get a browser-based testing environment, configured out of the box with [mocha](http://mochajs.org/), [chai](http://chaijs.com/), [async](https://github.com/caolan/async), [lodash](https://lodash.com/), [sinon](http://sinonjs.org/) & [sinon-chai](https://github.com/domenic/sinon-chai), [test-fixture](https://github.com/PolymerElements/test-fixture), [accessibility-developer-tools](https://github.com/GoogleChrome/accessibility-developer-tools). WCT will run your tests against whatever browsers you have installed locally, or remotely via Sauce Labs.

## Simple example using web-component-tester
# installing

    npm install web-component-tester --save-dev

# setting up

wct.conf.js

    module.exports = {
        verbose: true,
        plugins: {
            local: {
                browsers: ['chrome']
            }
        }
    };

# running

    node node_modules/web-component-tester/bin/wct

# test/index.html

    <html lang="en">
        <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, minimum-scale=1.0, initial-scale=1">
        
            <script src="../bower_components/webcomponentsjs/webcomponents.min.js"></script>
            <script src="../node_modules/web-component-tester/browser.js"></script>
            <link rel="import" href="../src/utils.html">
        
        </head>
        <body>
        
            <my-utils id="utils"></my-utils>
        
            <script>
        
                test('utils.isNumeric', function(){
                    var utils = document.getElementById('utils');
                    [1,0,-1,1.83,-9.87].forEach(function(d){
                        assert.isTrue(utils.isNumeric(d));
                    });
                    [true, false, null, {}, 'a', new Date()].forEach(function(d){
                        assert.isFalse(utils.isNumeric(d));
                    });
                });
        
            </script>
        
        </body>
    </html>

# src/utils.html

    <link rel="import" href="../bower_components/polymer/polymer.html">
    
    <dom-module id="my-utils">
        <template></template>
    </dom-module>
    
    Polymer({
    
        is: "my-utils",
    
        isNumeric: function(n) {
            return !isNaN(parseFloat(n)) && isFinite(n);
        }
    
    });

