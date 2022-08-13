---
title: "Continuous Deployment to Galaxy from Codeship"
slug: "continuous-deployment-to-galaxy-from-codeship"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This topic is heavily inspired by [Nate Strausers Migrating Meteor Apps from Modulus to Galaxy with Continuous Deployment from Codeship](https://medium.com/@natestrauser/migrating-meteor-apps-from-modulus-to-galaxy-with-continuous-deployment-from-codeship-aed2044cabd9#.o6huyy9w2).

## Setup
- Create a `deployment_token.json`: 

        METEOR_SESSION_FILE=deployment_token.json meteor login

- Create the following environment variables on Codeship: (https://codeship.com/projects/PROJECT_NUMBER/configure_environment)
    - METEOR_TARGET: your.domain.com
    - METEOR_TOKEN: Copy/Paste the contents of deployment_token.json. Something like: ```{"sessions": {"www.meteor.com": {"session": "12345 ...```
    - METEOR_SETTING: Copy/Paste the contents of your settings.json. Something like: ```{"private": {... ```
- Create a new deployment pipeline here https://codeship.com/projects/YOUR_PROJECT_NUMBER/deployment_branches/new
    - We deploy only the master branch. So set: Branch is exactly: master.
- Add a "Custom Script" as your deployment with the following content:
```
echo $METEOR_TOKEN > deployment_token.json
echo $METEOR_SETTINGS > deployment_settings.json
meteor npm prune --production
DEPLOY_HOSTNAME=galaxy.meteor.com METEOR_SESSION_FILE=deployment_token.json meteor deploy $METEOR_TARGET --settings deployment_settings.json
```



