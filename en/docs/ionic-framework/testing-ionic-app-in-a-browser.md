---
title: "Testing Ionic App in a Browser"
slug: "testing-ionic-app-in-a-browser"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

Testing of native device features like Camera, Vibration and others, many of which are found in the documentation of [Ionic Native][1], cannot be done in the browser. This is an inherent limitation of the fact that Cordova, the platform on which Ionic depends to be able to access native Android, iOS, and Windows Mobile APIs of a device, cannot run on the browser.

One can work around this issue by mocking the functionality of the native plugin.

Example
-------

Here's an example on how to mock the [`Camera`][2] plugin:

Go ahead and create an optional folder in your project root folder.

    cd src
    mkdir mocks 
    cd mocks 
    touch camera-mock.ts 

Open camera-mock.ts and copy paste the following code:

<!-- language: typescript -->

    export class CameraMock {
        getPicture(params) {
            return new Promise((resolve, reject) => {
                resolve("BASE_64_IMAGE_DATA");
            });
        }
    }

Next open `src/app.module.ts` and import the mock class"

<!-- language: typescript -->

    import { CameraMock } from "../mocks/camera-mock";

Then add it to module providers array:

<!-- language: typescript -->

    @NgModule({
    declarations: [
        MyApp,
        HomePage
    ],
    imports: [
        BrowserModule,
        IonicModule.forRoot(MyApp)
    ],
    bootstrap: [IonicApp],
    entryComponents: [
        MyApp,
        HomePage
    ],
    providers: [
        StatusBar,
        SplashScreen,
        CameraMock,
        {provide: ErrorHandler, useClass: IonicErrorHandler}
    ]
    })
    export class AppModule {}

Now you can use it in any component after importing it.

  [1]: https://ionicframework.com/docs/native/
  [2]: https://ionicframework.com/docs/native/camera/

## Testing in a Browser
Use `ionic serve` to start a local development server for app dev and testing. This is useful for both desktop browser testing, and to test within a device browser which is connected to the same network. Additionally, this command starts LiveReload which is used to monitor changes in the file system. As soon as you save a file the browser is refreshed automatically. Take a look at the [Sass docs](/docs/cli/sass.html) if you would also like to have `ionic serve` watch the project’s Sass files.

    $ ionic serve [options]
For chrome browser you can inspect devices (AVD or Mobiles), type following command in address bar of chrome browser.
    
    chrome://inspect/#devices  

### LiveReload

By default, LiveReload will watch for changes in your `www/` directory, excluding `www/lib/`. To change this, you can specify a `watchPatterns` property in the `ionic.project` file located in your project root to watch (or not watch) for specific changes.

    {
      "name": "myApp",
      "app_id": "",
      "watchPatterns": [
        "www/js/*",
        "!www/css/**/*"
      ]
    }

For a reference on glob pattern syntax, check out [globbing patterns](http://gruntjs.com/configuring-tasks#globbing-patterns) on the Grunt website.

Note: 

    $ ionic setup sass

will add a `watchPatterns` propery with the default values to your `ionic.project` file that you can then edit, in addition to the `gulpStartupTasks` property as described in the [Sass documentation](/docs/cli/sass.html).

### Ionic Lab

![Ionic Lab](http://ionicframework.com/img/blog/lab.png)

Ionic Lab is a feature on top of `ionic serve` that makes it easy to test your app in a phone frame and with iOS and Android platforms side-by-side. To use it, just run

    $ ionic serve --lab

Read the [full release announcement](http://ionicframework.com/blog/ionic-lab/) for all the details!

### Specifying an IP Address to use

Say you want to specify what address your browser will connect to, say for testing or external users. Specify the address with the `--address` argument.

    $ ionic serve --address 68.54.96.105

### Service Proxies

The `serve` command can add some proxies to the http server. These proxies are useful if you are developing in the browser and you need to make calls to an external API. With this feature you can proxy request to the external api through the ionic http server preventing the `No 'Access-Control-Allow-Origin' header is present on the requested resource` error.

In the `ionic.project` file you can add a property with an array of proxies you want to add. The proxies are object with two properties:

*   `path`: string that will be matched against the beginning of the incoming request URL.
*   `proxyUrl`: a string with the url of where the proxied request should go.

<div class="language-json highlighter-rouge">

    {
      "name": "appname",
      "email": "",
      "app_id": "",
      "proxies": [
        {
          "path": "/v1",
          "proxyUrl": "https://api.instagram.com/v1"
        }
      ]
    }

</div>

Using the above configuration, you can now make requests to your local server at `http://localhost:8100/v1` to have it proxy out requests to `https://api.instagram.com/v1`

For example:

    angular.module('starter.controllers', [])
    .constant('InstagramApiUrl', '')
    // .constant('InstagramApiUrl','https://api.instagram.com')
    //In production, make this the real URL

    .controller('FeedCtrl', function($scope, $http, InstagramApiUrl) {

      $scope.feed = null;

      $http.get(InstagramApiUrl + '/v1/media/search?client_id=1&lat=48&lng=2.294351').then(function(data) {
        console.log('data ' , data)
        $scope.feed = data;
      })

    })

See also [this gist](https://gist.github.com/jbavari/d9c1c94058c4fdd4e935) for more help.

### Command-line flags/options

    [--consolelogs|-c] ......  Print app console logs to Ionic CLI
    [--serverlogs|-s] .......  Print dev server logs to Ionic CLI
    [--port|-p] .............  Dev server HTTP port (8100 default)
    [--livereload-port|-i] ..  Live Reload port (35729 default)
    [--nobrowser|-b] ........  Disable launching a browser
    [--nolivereload|-r] .....  Do not start live reload
    [--noproxy|-x] ..........  Do not add proxies

