---
title: "Getting Started with Ember-Cli and Deployments"
slug: "getting-started-with-ember-cli-and-deployments"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
 - ember deploy production // deploy production environment
 - ember deploy staging // deploy staging environment
 - ember deploy development // deploy development environment which is not compress and minified 

## Parameters
| parameters | details |
| ------ | ------ |
| `ember help`   | show all possible params and depth guide as well as shortcodes   |

Ember-Cli is a powerful tool which comes with many others to help us deploying faster and convenient. All you need to install [Ember-Cli-Deploy][1] and use `ember deploy`.

> Ember CLI Deploy structures your app’s deployment using a deploy
> pipeline, which consists of several pipeline hooks. These standard
> hooks are the foundation for a rich ecosystem of plugins which you can
> compose to create a deployment process suitable for your application.

As Ember-Cli-Deploy is an addon of Ember so you can easily install that with `ember install ember-cli-deploy`. There are two useful other add-ons which make our build and compressing reliable during deployment. 

Simply run the following commands:

    # Install the Build plugin, which builds your app during deployment
    ember install ember-cli-deploy-build
    
    # Gzip our files
    ember install ember-cli-deploy-gzip

However, if you are going to maximize your benefits using `ember deploy`, it's most likey to have different environments in your Ember application and deploy production,staging or development version of your app with the appropriate configuration. 

Platforms that you can deploy by now are:

 - Heroku
 - Azure
 - AWS S3
 - Firebase
 - CouchDB cluster

Kindly refer to example section to see how you can deploy. 



  [1]: http://ember-cli-deploy.com/

## AWS S3
To proceed with the deployment to S3, we will install these plugins:

 - [ember-cli-deploy-s3-index][1]: It uploads the index.html to S3 with revision information, and activates it
 - [ember-cli-deploy-s3][2]: It uploads the assets (js, css and other media files) to S3

As they are Ember addon you can easily install by running the following commands 

    ember install ember-cli-deploy-s3-index
    ember install ember-cli-deploy-s3

All you need after that is to configure deploy.js file which should under /config folder:


    // config/deploy.js
    
    module.exports = function(deployTarget) {
    
      var ENV = {
        build: {
          environment: deployTarget
        },
        'revision-data': {
          type: 'git-commit'
        },
        's3-index': {
          accessKeyId: process.env['S3_ACCESS_KEY'],
          secretAccessKey: process.env['S3_SECRET_ACCESS_KEY'],
          bucket: "your-app-deployment-bucket",
          region: "YOUR REGISION",
          allowOverwrite: true // if you want to overwrite index file if not change it to false
        },
        's3': {
          accessKeyId: process.env['S3_ACCESS_KEY'],
          secretAccessKey: process.env['S3_SECRET_ACCESS_KEY'],
          bucket: "your-app-deployment-bucket",
          region: "YOUR REGISION",
        }
      };
    
      return ENV;
    
    };

Notice that we have used the environmental variables within our config.If you need to read configuration from a file, it’s also possible to return a promise that resolves with the `ENV` object. Here is an example to define different environments in deploy.js file:

    if (deployTarget === 'development') {
        ENV.build.environment = 'development';
        // configure other plugins for development deploy target here
      }
    
      if (deployTarget === 'staging') {
        ENV.build.environment = 'production';
        // configure other plugins for staging deploy target here
      }
    
      if (deployTarget === 'production') {
        ENV.build.environment = 'production';
        // configure other plugins for production deploy target here
      }

Finally, you can easily run following command to deploy 

    ember deploy [YOUR APP ENVIRONMENT] //e.g-> ember deploy production or ember deploy staging

if you like to see details you can run:

    ember deploy production --verbose --activate=true

  [1]: https://github.com/ember-cli-deploy/ember-cli-deploy-s3-index
  [2]: https://github.com/ember-cli-deploy/ember-cli-deploy-s3


## Heroku
You must have installed [Heroku Toolbelt][1] first. Having an account in Heroku and installation of ember-cli-deploy are mandatory. 

Creating a new Heroku instance from an Ember CLI application's parent directory:

    $ heroku create --buildpack https://github.com/tonycoco/heroku-buildpack-ember-cli.git
    
    $ git push heroku master



  [1]: http://%20https://toolbelt.heroku.com/

## Azure
Firstly, it's required to install Microsoft’s module [ember-cli-azure-deploy][1]. You need to be in your application root directory. 

    npm install --save-dev -g ember-cli-azure-deploy
    azure-deploy init

If you are using Yarn package manager you can simply install by:

    yarn global add ember-cli-azure-deploy
    azure-deploy init

This will create a `deploy.sh` in your project's root folder, enabling Azure to follow a set of instructions - including installing all the required Node Modules, running `ember build` and deploying the resulting `dist/` folder to your website's `wwwroot`.


  [1]: https://github.com/felixrieseberg/ember-cli-azure-deploy

## Firebase
First, you need to install [Firebase tools][1]. Simply, run the commands below:

Npm Package manager

    npm install -g firebase-tools 
or Yarn package manager

    yarn add firebase-tools

To configure your application to be ready to deploy you need to run the following in your app’s root directory:

    firebase init

finally, by running the following command you can deploy your application

    firebase deploy


  [1]: http://%20https://github.com/firebase/firebase-tools

