---
title: "MVC Structure"
slug: "mvc-structure"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

MVC stands for Model-View-Controller. Any application that separates it’s data access, business logicand user interface is called MVC. There can be two types of MVC: convention-based and configuration-based. Example, cakePHP is convention-based, i.e. you just need to follow the instructions of the core system to get your module ready in just few lines. Magento is configuration-based, i.e. you need to specify each and every thing to your module’s config file in order to get it work. Magento has Controller (for Routing), Block, Model and Template file.
How Magento’s MVC works:
1. When you enter the URL (something like http://mysite.com/frontname/controller/method/param1/value1/param2/value2), this URL is intercepted by one PHP file called index.php which instantiates Magento application
2. Magento application instantiates Front Controller object
3. Further, front controller instantiates Router objects (specified in module’s config.xml, global tag)
4. Now, Router is responsible to “match” the frontname which is in our URL
5. If “match” is found, it sees controller name and method name in the URL, which is finally called.
6. Now depending on what is written in action name (method name), it is executed. If any models are called in it, the controller method will instantiate that model and call the method in it which is requested.
7. Then the controller action (method) instantiate the Layout object, which calls Block specified for this action (method) name (Each controller action name have block and template file associated with it, which can be found at app/design/frontend or adminhtml/namespace/module/layout/module.xml file, name of layout file (module.xml) can be found in config.xml of that module, in layout updates tag).
8. Template file (.phtml) now calls the corresponding block for any method request. So, if you write $this->methodName in .phtml file, it will check “methodName” in the block file which is associated in module.xml file.
9. Block contains PHP logic. It references Models for any data from DB.
10. If either Block, Template file or Controller need to get/set some data from/to database, they can call Model directly like Mage::getModel(‘modulename/modelname’).

## Understand MVC in Magento
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Xs23A.png

## MVC Flow in Magento
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/M0EvX.png

