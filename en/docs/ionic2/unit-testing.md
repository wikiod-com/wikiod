---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Unit Testing in general gives additional safety to a product to prevent issues when modifying/adding features. A safety net that says "EVERYTHING STILL WORKS". Unit Tests do not replace in any way the actual user tests that a proper QA can do.

In this document we will base the examples on this repository: https://github.com/driftyco/ionic-unit-testing-example

## Unit Tests with Karma/Jasmine
Unit testing in ionic is the same as in any angular app. 

We'll be using a few frameworks to do this.

Karma - a framework for running tests

Jasmine - a framework for writing tests 

PhantomJS - an application that runs javascript without a browser

First of all lets install everything, so make sure your package.json includes these lines in the dev dependencies. I feel its important to note that that dev dependencies don't affect your app at all and are just there to help the developer.

    "@ionic/app-scripts": "1.1.4",
    "@ionic/cli-build-ionic-angular": "0.0.3",
    "@ionic/cli-plugin-cordova": "0.0.9",
    "@types/jasmine": "^2.5.41",
    "@types/node": "^7.0.8",
    "angular2-template-loader": "^0.6.2",
    "html-loader": "^0.4.5",
    "jasmine": "^2.5.3",
    "karma": "^1.5.0",
    "karma-chrome-launcher": "^2.0.0",
    "karma-jasmine": "^1.1.0",
    "karma-jasmine-html-reporter": "^0.2.2",
    "karma-sourcemap-loader": "^0.3.7",
    "karma-webpack": "^2.0.3",
    "null-loader": "^0.1.1",
    "ts-loader": "^2.0.3",
    "typescript": "2.0.9"

To go over packages a bit

    "angular2-template-loader": "^0.6.2", - will load and compile the angular2 html files.

    "ts-loader": "^2.0.3", - will compile the actual typescript files

    "null-loader": "^0.1.1", - will not load the assets that will be missing, such as fonts and images. We are testing, not image lurking.

We should also add this script to our package.json scripts:

    "test": "karma start ./test-config/karma.conf.js"
Also take note in tsconfig that you are excluding the spec.ts files from compilation:
 

     "exclude": [
        "node_modules",
        "src/**/*.spec.ts"
      ],
Ok, now lets take the actual testing configuration.
Create a `test-config` folder in your project folder. (Just as it was mentioned in the package.json script)
Inside the folder create 3 files:

`webpack.test.js` - which will tell the webpack what files to load for the testing process

    var webpack = require('webpack');
    var path = require('path');
    
    module.exports = {
      devtool: 'inline-source-map',
    
      resolve: {
        extensions: ['.ts', '.js']
      },
    
      module: {
        rules: [
          {
            test: /\.ts$/,
            loaders: [
              {
                loader: 'ts-loader'
              } , 'angular2-template-loader'
            ]
          },
          {
            test: /\.html$/,
            loader: 'html-loader'
          },
          {
            test: /\.(png|jpe?g|gif|svg|woff|woff2|ttf|eot|ico)$/,
            loader: 'null-loader'
          }
        ]
      },
    
      plugins: [
        new webpack.ContextReplacementPlugin(
          // The (\\|\/) piece accounts for path separators in *nix and Windows
          /angular(\\|\/)core(\\|\/)(esm(\\|\/)src|src)(\\|\/)linker/,
          root('./src'), // location of your src
          {} // a map of your routes
        )
      ]
    };
    
    function root(localPath) {
      return path.resolve(__dirname, localPath);
    }

`karma-test-shim.js` - which will load the angular related libraries, such as zone and test libraries as well as configure the module for testing.

    Error.stackTraceLimit = Infinity;
    
    require('core-js/es6');
    require('core-js/es7/reflect');
    
    require('zone.js/dist/zone');
    require('zone.js/dist/long-stack-trace-zone');
    require('zone.js/dist/proxy');
    require('zone.js/dist/sync-test');
    require('zone.js/dist/jasmine-patch');
    require('zone.js/dist/async-test');
    require('zone.js/dist/fake-async-test');
    
    var appContext = require.context('../src', true, /\.spec\.ts/);
    
    appContext.keys().forEach(appContext);
    
    var testing = require('@angular/core/testing');
    var browser = require('@angular/platform-browser-dynamic/testing');
    
    testing.TestBed.initTestEnvironment(browser.BrowserDynamicTestingModule, browser.platformBrowserDynamicTesting());

`karma.conf.js` - defines the configuration of how to test with karma. Here you can switch from Chrome to PhantomJS to make this process invisible and faster among other things.

    var webpackConfig = require('./webpack.test.js');
    
    module.exports = function (config) {
      var _config = {
        basePath: '',
    
        frameworks: ['jasmine'],
    
        files: [
          {pattern: './karma-test-shim.js', watched: true}
        ],
    
        preprocessors: {
          './karma-test-shim.js': ['webpack', 'sourcemap']
        },
    
        webpack: webpackConfig,
    
        webpackMiddleware: {
          stats: 'errors-only'
        },
    
        webpackServer: {
          noInfo: true
        },
    
        browserConsoleLogOptions: {
          level: 'log',
          format: '%b %T: %m',
          terminal: true
        },
    
        reporters: ['kjhtml', 'dots'],
        port: 9876,
        colors: true,
        logLevel: config.LOG_INFO,
        autoWatch: true,
        browsers: ['Chrome'],
        singleRun: false
      };
    
      config.set(_config);
    };

Now that we configured everything lets write some actual test.
For this example we will write an app.component spec file.
If you would like to see tests for a page and not the main component you can look here: https://github.com/driftyco/ionic-unit-testing-example/blob/master/src/pages/page1/page1.spec.ts

What we need to do first is to test out our constructor.
This will create and run the constructor of our app.component

      beforeEach(async(() => {
        TestBed.configureTestingModule({
          declarations: [MyApp],
          imports: [
            IonicModule.forRoot(MyApp)
          ],
          providers: [
            StatusBar,
            SplashScreen
          ]
        })
      }));

The declaration will include our main ionic app. The Imports will the imports needed for this test. Not everything.

The providers will include the things that are injected in to the constructor but are not part of the import. For instance the app.component injects the Platform service but since its a part of the IonicModule there is no need to mention it in the providers.

For the next tests we will need to get an instance of our component:

      beforeEach(() => {
        fixture = TestBed.createComponent(MyApp);
        component = fixture.componentInstance;
      });

Next a few tests to see that everything is in order:


      it ('should be created', () => {
        expect(component instanceof MyApp).toBe(true);
      });
    
      it ('should have two pages', () => {
        expect(component.pages.length).toBe(2);
      });

So in the end we will have something like this:

    import { async, TestBed } from '@angular/core/testing';
    import { IonicModule } from 'ionic-angular';
    
    import { StatusBar } from '@ionic-native/status-bar';
    import { SplashScreen } from '@ionic-native/splash-screen';
    
    import { MyApp } from './app.component';
    
    describe('MyApp Component', () => {
      let fixture;
      let component;
    
      beforeEach(async(() => {
        TestBed.configureTestingModule({
          declarations: [MyApp],
          imports: [
            IonicModule.forRoot(MyApp)
          ],
          providers: [
            StatusBar,
            SplashScreen
          ]
        })
      }));
    
      beforeEach(() => {
        fixture = TestBed.createComponent(MyApp);
        component = fixture.componentInstance;
      });
    
      it ('should be created', () => {
        expect(component instanceof MyApp).toBe(true);
      });
    
      it ('should have two pages', () => {
        expect(component.pages.length).toBe(2);
      });
    
    });

Run the tests by 

    npm run test

And that's about it for the basic testing.
There are a few ways to shortcut the test writing like writing your own TestBed and having inheritance in tests which might help you out in the long run.



