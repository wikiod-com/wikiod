---
title: "SalesForce CI Integration"
slug: "salesforce-ci-integration"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Place to use Jenkins and Sonar for CI

## How to configure Jenkins to deploy code on Development or Production org ?
How we can use jenkins in our SalesForce product development.
What are the tools plugins are available for Jenkins Integration
How to solve CI configuration issue.....etc

## Jenkins CI tools which can be used for SalesForce Automation
 1. [Jenkins][1]: The leading open source automation server, Jenkins provides hundreds of plugins to support building, deploying and automating any project.
 2. [Sonar Qube][2]: SonarQube provides the capability to not only show health of an application but also to highlight issues newly introduced.
 3. [Apache Ant][3]: Apache Ant is a Java library and command-line tool whose mission is to drive processes described in build files as targets and extension points dependent upon each other.
 4. [Apache Maven][4]: Apache Maven is a software project management and comprehension tool. Based on the concept of a project object model (POM), Maven can manage a project's build, reporting and documentation from a central piece of information.
 5. [SfApexDoc][5]: Support for JavaDoc like documentation creation tool. Can be used by Ant/Jenkins to create Documents.
 6. [JUnit format Report for APEX][6]: Extends the Force.com com.salesforce.ant.DeployTask to accept an optional junitreportdir argument that defines the folder that a JUnitReport XML file is output into. This file can be consumed directly by the Jenkins continuous integration tool to produce trend graphs and test result details or by the JUnitReport Ant task.
 7. Version Control System: Can use **GIT**, **SVN** or any other Version Control system 
 8. [PMD Apex][7]: Contains the PMD implementation to support the Apex programming language.
 9. [Sonar for Apex(enforce-sonarqube-plugin)][8]: The plugin has support for the Apex language grammar, the current list of checks is focused mainly on test components. The support for more SFDC components is in progress.

 


 


  [1]: https://jenkins.io/
  [2]: https://www.sonarqube.org
  [3]: http://ant.apache.org/
  [4]: http://maven.apache.org/
  [5]: http://force-code.com/sfapexdoc/
  [6]: https://github.com/beamso/force-deploy-with-xml-report-task
  [7]: http://pmd.sourceforge.net/snapshot/pmd-apex/
  [8]: https://github.com/fundacionjala/enforce-sonarqube-plugin

