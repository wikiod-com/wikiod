---
title: "Deploying Sample application using Spring-boot on Amazon Elastic Beanstalk"
slug: "deploying-sample-application-using-spring-boot-on-amazon-elastic-beanstalk"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Deploying sample application using Spring-boot in Jar format on AWS
 1. Create a sample application using spring-boot from [spring-boot initializer][1] site.
 2. Import the code in your local IDE and run the goal as **clean install spring-boot:run -e**
 3. Go to target folder and check for the jar file.
 4. Open your Amazon account or create a new Amazon Account and select for the Elastic Beanstalk as shown in the below [![image][2]][2]. 
 5. Create a new web server environment as shown in below [![figure][3]][3].
 6. Select the Environment type as Java for **JAR** file deployment for Spring-boot, if you are planning to deploy as a **WAR** file, it should be selected as tomcat as shown in below [![figures][4]][4].
[![figure2][5]][5]
 7. Select with Default configuration's upon clicking next next ...
 8. Once you complete the default configuration's, in the overview screen the JAR file can be uploaded and deployed as shown in the figures.
[![uploadanddeploy][6]][6]
[![deploy][7]][7]
 9. Once the Deployment is successful (5 -10 minutes for the first time)
  you can hit the context url as shown in the figure below.
[![contexturl][8]][8]
 
 10. Result is as shown below,it should work as same as with your local env.

[![Env][9]][9]

 11. Please find my [Github URL][10]


  [1]: https://start.spring.io/
  [2]: http://i.stack.imgur.com/TEys0.png
  [3]: http://i.stack.imgur.com/R9Xh2.png
  [4]: http://i.stack.imgur.com/0AIjj.png
  [5]: http://i.stack.imgur.com/fDWVZ.png
  [6]: http://i.stack.imgur.com/SRUap.png
  [7]: http://i.stack.imgur.com/pasDz.png
  [8]: http://i.stack.imgur.com/p68LF.png
  [9]: http://i.stack.imgur.com/soXoe.png
  [10]: https://github.com/Praveenmail2him/awsjardeployspringboot

