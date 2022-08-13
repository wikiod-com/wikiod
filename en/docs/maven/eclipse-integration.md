---
title: "Eclipse integration"
slug: "eclipse-integration"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Install Maven in Eclipse
You can take advantage of Apache Maven's powerful features in Eclipse by installing the [M2Eclipse][1] feature. Follow these steps to install Maven in Eclipse: 

 1. Open Eclipse and select _Help_ → _Install New Software…_
 2. In the opened dialog, select the <kbd>Add...</kbd> button to add a new repository. 
 3. Fill in the form with the information below and confirm with <kbd>OK</kbd>:
    
    _Name:_ `M2Eclipse`

    _Location:_ `http://download.eclipse.org/technology/m2e/releases`

 4. After the _Pending..._ finishes, <kbd>Select All</kbd> and select <kbd>Next</kbd>.
 5. ◉ accept the terms of the license agreement and select <kbd>Finish</kbd>.
 6. At the end of the installation you will be asked to restart Eclipse. Select <kbd>Yes</kbd> to perform the restart.

[1]: http://www.eclipse.org/m2e/

## Check if Eclipse already has M2Eclipse Maven support installed
Go to _Help_ → _About Eclipse_ → Check if the _[m2e feature][1]_ is there: **[![Image link][2]][2]**.

  [1]: https://www.eclipse.org/m2e/
  [2]: https://github.com/eclipse/m2e-core/blob/master/org.eclipse.m2e.core.ui/icons/m2eclipse48.gif
       <!-- Failed to upload image; the format is not supported -->




## Configure a custom Maven installation in Eclipse
Eclipse would provide its own embedded Maven enviroment out-of-the-box, which is not recommended whenever a certain Maven version must be used or further configuration must be performed (proxy, mirrors and so on): that is, to have full control over which Maven environment would be used by the IDE.

* Select _Window_ → _Preferences_ → _Maven_ → _Installations_
* Select <kbd>Add..</kbd> to add a custom/local Maven installation
* Supply the necessary information and select <kbd>Finish</kbd>:

  _Installation home:_ `... your Maven home ...` <kbd>Directory...</kbd>

  _Installation name:_ `Apache Maven x.y.z`

* ☑ select it as default (instead of the default _EMBEDDED_ version) and confirm with <kbd>OK</kbd>.

