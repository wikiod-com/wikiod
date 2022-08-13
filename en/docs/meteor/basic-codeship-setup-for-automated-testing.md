---
title: "Basic Codeship Setup for Automated Testing"
slug: "basic-codeship-setup-for-automated-testing"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Setup Codeship
- Go to [Codeship.com](https://codeship.com) and create an account (or login)
- Create a new project
- Import your project via Github or Bitbucket
- On the screen "Configure Your Tests" use these commands:
    - Select "I want to create my own custom commands" from the "Select your technology to prepopulate basic commands" dropdown.
    - Enter the following commands:
        <!-- language: lang-sh -->
            curl -o meteor_install_script.sh https://install.meteor.com/
            chmod +x meteor_install_script.sh
            sed -i "s/type sudo >\/dev\/null 2>&1/\ false /g" meteor_install_script.sh
            ./meteor_install_script.sh
            export PATH=$PATH:~/.meteor/
            meteor --version
            meteor npm install

    - Leave the test commands like this:
        ```
        npm test
        ```
- Push a new commit to Github / Bitbucket
- That's it

## Prepare the Project
- [Write some tests](https://guide.meteor.com/testing.html)
- Install [dispatch:mocha-phantomjs](https://atmospherejs.com/practicalmeteor/mocha):
    ```
    meteor add dispatch:mocha-phantomjs
    ```
- Add a test-command to your package.json.
    ```
    {    
      "name": "awesome meteor package",
      "scripts": {
        "test": "meteor test --driver-package dispatch:mocha-phantomjs --once"
      }
    }
    ```
- Make sure that you can run ```npm test``` in your project root.
    

