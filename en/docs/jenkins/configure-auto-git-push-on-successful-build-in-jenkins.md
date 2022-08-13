---
title: "Configure Auto Git Push on Successful Build in Jenkins"
slug: "configure-auto-git-push-on-successful-build-in-jenkins"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

This document will take you through the steps to configure a Jenkins job that allows user to setup auto push on successful build.The push operation can be controlled by the user. User can choose if they want to perform the auto push operation on successful build or not. 

## Configuring the Auto Push Job
Create a build job (according to your requirement). For this example I have created a freestyle job (AutoPush) to perform ANT build.

We are going to create two variables, PUSH (Choice Parameter) and TAG_NUMBER (String Parameter).

We can choose the value YES or NO for PUSH, this will decide whether to push the code to a tag or not on successful build.

We can specify a tag name (ex. 1.0.1) for TAG_NUMBER to create a new tag (ex. 1.0.1) in the remote repository with the same name or specify an existing tag name to update an existing tag.

[![enter image description here][1]][1]
 










**Now let’s move on to the job configuration.** 
1.    Check the “This project is parameterized” checkbox and create a Choice Parameter called “PUSH” and provide YES and NO as the choices. This parameter will decide whether you want to push the code to a specific Tag/Release or not.

[![enter image description here][2]][2]
 







2.    Then create a String Parameter called “TAG_NUMBER”, using this parameter we can specify a new tag number to create a new tag or specify an existing tag number to update an existing tag.

[![enter image description here][3]][3]
 









3.    In Source Code Management section choose Git and provide the repository URL. This repository contains the source code that you are going to build and after a successful build a release tag will be created on the same repository.
 
[![enter image description here][4]][4]






4.    After adding the repository details click on advanced and provide a name to your repository which will later get referred in the Git Publisher plugin to identify the repository. 

[![enter image description here][5]][5]

[![enter image description here][6]][6]

 

 







5.    Then add the build step. In this example I am building an ANT project.

[![enter image description here][7]][7]
 











6.    Now in “Post-build Actions” section select “Flexi Publish” plugin. Select the value “And” from the dropdown for Conditional action (Run?). Then select “String Match” from the dropdown for the Run condition (&&).

[![enter image description here][8]][8]

[![enter image description here][9]][9]

[![enter image description here][10]][10]
 
 

 




7.    After selecting the string match specify $PUSH as String 1 value and YES as String 2 value.  So when you will run the build if you choose the value of PUSH as YES, it will compare the String 1 (=$PUSH) and String 2 (=YES) and trigger the Git push operation and if you choose NO it won’t trigger the Git push operation.
          

    Choose the value of PUSH -> YES OR NO -> Chosen value “YES”
    then, $PUSH = YES
    AS String 1 = $PUSH => String 1 = YES
    Again, String 2 = YES, hence String 2 == String 1 (String match)
    Then, trigger the Git push action. 

[![enter image description here][11]][11]

 





8.    Now click on Add dropdown option to add the Git publisher action that will be triggered on the basis of the string match condition.

[![enter image description here][12]][12]

[![enter image description here][13]][13]
 

 
9.    After selecting Git Publisher, do the configuration as follows:

[![enter image description here][14]][14]
 
After the configuration save the job and you are done.


  [1]: https://i.stack.imgur.com/U1hcD.png
  [2]: https://i.stack.imgur.com/cOUgN.png
  [3]: https://i.stack.imgur.com/tQh44.png
  [4]: https://i.stack.imgur.com/PgJJu.png
  [5]: https://i.stack.imgur.com/lyKCw.png
  [6]: https://i.stack.imgur.com/9ur7Z.png
  [7]: https://i.stack.imgur.com/9l98b.png
  [8]: https://i.stack.imgur.com/C2FYN.png
  [9]: https://i.stack.imgur.com/tccWc.png
  [10]: https://i.stack.imgur.com/NK1Y4.png
  [11]: https://i.stack.imgur.com/ILfu6.png
  [12]: https://i.stack.imgur.com/gdKSW.png
  [13]: https://i.stack.imgur.com/G83MK.png
  [14]: https://i.stack.imgur.com/uOhDL.png

