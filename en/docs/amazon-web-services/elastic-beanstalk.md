---
title: "Elastic Beanstalk"
slug: "elastic-beanstalk"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

**Current Limitations (As of 2016-10-03)**

 - Environment Tags cannot be changed once the *Environment* is created, so choose wisely.
- Autoscaling in Elastic Beanstalk is limited to *Simple* and *Scheduled*, so if you wish to use *Step-Scaling*, re-consider if Elastic Beanstalk is a good fit.

**Automation with Jenkins**

There is a great [AWSEB Deployment Plugin][1] for Jenkins that will plug-n-play for deployment to Elastic Beanstalk (blue/green deployments with automatic idle termination is just a checkbox away).


  [1]: https://wiki.jenkins-ci.org/display/JENKINS/AWSEB+Deployment+Plugin

## Introduction to Elastic Beanstalk
Elastic Beanstalk (EB) is essentially a hybrid between Golden AMIs and [CloudFormation][1], while vastly simplifying the learning curve of [Puppet][2] or [Chef][3].


----------


An Elastic Beanstalk deployment is broken down into two components: Application and Environment.

**Application**

Consider this your top-level grouping, your application itself. For example, a single application ("MyWebApp") may have multiple Environments ("Production" and "Staging").

**Environment**

Each environment will consist of a complete architecture deployment ([EC2 Instances][4], [Elastic Load Balancer][5], [Autoscaling Group][6], and [Cloudwatch Alarms][7]). The entire environment configuration is setup and maintained for your automatically.


----------


**Deploying an Application**

Your application deployment is as simple as uploading a zip file containing your code. Each zip file (called *Application Version*) you upload is associated to an *Application*, so you can upload once and deploy to multiple *Environments*.

**Customizing the Environment**

By default, Elastic Beanstalk will deploy "stock" Amazon-Maintained AMIs. For most applications, this is sufficient, but there may be environmental tweaks that you want to make (eg. changing the timezone, adding packages/dependencies not present in the code, etc).

There are two ways of customizing the EB AMI that is used: [ebextensions][8] or a custom AMI.


*ebextensions* - A folder, quite literally called '.ebextensions', that can optionally be placed at the root of your *Application Version* (the zip you uploaded containing your code). Within the ebextensions folder, you can place YAML files defining any custom scripts, dependencies, etc that you want executed server-side during the deployment process. There are a number of hooking points available, for the latest information, please check the relevant documentation: http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/ebextensions.html

----------


**Gotchas / Things to be aware of**

*VPC Checkbox* - When creating an environment, the option is discretely made available as to whether or not the environment should be created/placed within a VPC. If you need your application to communicate with existing resources that you have created, CHECK THIS BOX. Otherwise, Elastic Beanstalk will create a new security group that is isolated from the rest of your VPC. *While you will be able to manually adjust the security group settings after creation, trying to essentially 'add' it into a VPC will cause a variety of problems later on.*

*RDS* - When creating an environment, you have the option to create an RDS instance as part of the environment. It is not recommended to use this, as anytime you need to 'rebuild' the environment (eg. blue/green deployments, troubleshooting) it will destroy and recreate the RDS instance (along with all data).


  [1]: https://aws.amazon.com/cloudformation/
  [2]: https://puppet.com/
  [3]: https://www.chef.io/chef/
  [4]: https://aws.amazon.com/ec2/
  [5]: https://aws.amazon.com/elasticloadbalancing/
  [6]: https://aws.amazon.com/autoscaling/
  [7]: https://aws.amazon.com/cloudwatch/
  [8]: http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/ebextensions.html

## Blue/Green Deployments on Elastic Beanstalk
Blue/Green deployment is a release technique that reduces downtime and risk by running two identical production environments (one called "Blue", the other called "Green"). At any one time, only one of the environments is serving live traffic, while the other is sitting idle.

When deploying a new version, the code is deployed to the idle environment (eg. Green) and after confirming a successful deployment, live traffic is switched (eg. Green has new code, traffic from Blue is now routed to Green). The next code deploy will occur on the new idle environment (following the example, that would now be Blue).

**Example/Visual Aid**:

[![blue/green deployment example][1]][1]

*image source: https://cloudnative.io/statics/blog/aws-blue-green-deployment.png*


----------

When using Elastic Beanstalk (EB), you can easily create multiple environments that are exact clones of one another for code deployment. After confirming that the new environment is ready to go live, it is as simple as using the "Swap URLs" function in EB to swap environments.

Step-by-step instructions: http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.CNAMESwap.html


  [1]: http://i.stack.imgur.com/OLqhQ.png

