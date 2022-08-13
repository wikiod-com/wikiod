---
title: "Setting up Jenkins for iOS build automation."
slug: "setting-up-jenkins-for-ios-build-automation"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Now you can define Continuous Integration and Continuous Delivery (**CI/CD**) process as code with Jenkins 2.0 for your projects in iOS 10. Activities like to build, test, code coverage, check style, reports, and notifications can be described in only one file.

To read the complete article go to [Pipeline in Jenkins 2.0 as Code for iOS 10 and XCode 8][1]


  [1]: http://mmorejon.github.io/en/blog/build-pipeline-jenkins2-as-code-with-ios10-xcode8/

## Parameters
| Parameter | Details |
| ------ | ------ |
| node('iOS Node')    | Jenkins Node with Mac OS. If Jenkins is installed in Mac OS use `node {....}`   |

The article is written in both languages: English and Spanish.


## Time Table Example
The source code can be [cloned or downloaded from GitHub][1] to test it.

```
node('iOS Node') {

    stage('Checkout/Build/Test') {

        // Checkout files.
        checkout([
            $class: 'GitSCM',
            branches: [[name: 'master']],
            doGenerateSubmoduleConfigurations: false,
            extensions: [], submoduleCfg: [],
            userRemoteConfigs: [[
                name: 'github',
                url: 'https://github.com/mmorejon/time-table.git'
            ]]
        ])

        // Build and Test
        sh 'xcodebuild -scheme "TimeTable" -configuration "Debug" build test -destination "platform=iOS Simulator,name=iPhone 6,OS=10.1" -enableCodeCoverage YES | /usr/local/bin/xcpretty -r junit'

        // Publish test restults.
        step([$class: 'JUnitResultArchiver', allowEmptyResults: true, testResults: 'build/reports/junit.xml'])
    }

    stage('Analytics') {
        
        parallel Coverage: {
            // Generate Code Coverage report
            sh '/usr/local/bin/slather coverage --jenkins --html --scheme TimeTable TimeTable.xcodeproj/'
    
            // Publish coverage results
            publishHTML([allowMissing: false, alwaysLinkToLastBuild: false, keepAll: false, reportDir: 'html', reportFiles: 'index.html', reportName: 'Coverage Report'])
        
            
        }, Checkstyle: {

            // Generate Checkstyle report
            sh '/usr/local/bin/swiftlint lint --reporter checkstyle > checkstyle.xml || true'
    
            // Publish checkstyle result
            step([$class: 'CheckStylePublisher', canComputeNew: false, defaultEncoding: '', healthy: '', pattern: 'checkstyle.xml', unHealthy: ''])
        }, failFast: true|false   
    }

    stage ('Notify') {
        // Send slack notification
        slackSend channel: '#my-team', message: 'Time Table - Successfully', teamDomain: 'my-team', token: 'my-token'
    }
}
```


  [1]: https://github.com/mmorejon/time-table

