---
title: "Continuous Integration With Jenkins"
slug: "continuous-integration-with-jenkins"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## Jenkins 2.0+ Pipeline Script
Modern versions of Jenkins (version 2.x) come with a "Build Pipeline Plugin" that can be used to orchestrate complex CI tasks without creating a multitude of interconnected jobs, and allow you to easily version-control your build / test configuration.

You may install this manually in a "Pipeline" type job, or, if your project is hosted on Github, you may use the "GitHub Organization Folder Plugin" to automatically set up jobs for you.

Here's a simple configuration for Django sites that require only the site's specified python modules to be installed.

    #!/usr/bin/groovy
    
    node {
      // If you are having issues with your project not getting updated, 
      // try uncommenting the following lines.
      //stage 'Checkout'
      //checkout scm
      //sh 'git submodule update --init --recursive'
    
      stage 'Update Python Modules'
      // Create a virtualenv in this folder, and install or upgrade packages
      // specified in requirements.txt; https://pip.readthedocs.io/en/1.1/requirements.html
      sh 'virtualenv env && source env/bin/activate && pip install --upgrade -r requirements.txt'
      
      stage 'Test'
      // Invoke Django's tests
      sh 'source env/bin/activate && python ./manage.py runtests'
    }

## Jenkins 2.0+ Pipeline Script, Docker Containers
Here is an example of a pipeline script that builds a Docker container, then runs the tests inside of it. The entrypoint is assumed to be either `manage.py` or `invoke`/`fabric` with a `runtests` command available.

    #!/usr/bin/groovy
    
    node {
      stage 'Checkout'
      checkout scm
      sh 'git submodule update --init --recursive'
    
      imageName = 'mycontainer:build'
      remotes = [
        'dockerhub-account',
      ]
    
      stage 'Build'
      def djangoImage = docker.build imageName
    
      stage 'Run Tests'
      djangoImage.run('', 'runtests')
    
      stage 'Push'
      for (int i = 0; i < remotes.size(); i++) {
          sh "docker tag ${imageName} ${remotes[i]}/${imageName}"
          sh "docker push ${remotes[i]}/${imageName}"
       }
    }

