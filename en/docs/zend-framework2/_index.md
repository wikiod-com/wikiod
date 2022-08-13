---
title : zend-framework2 Tutorial
slug : zend-framework2-tutorial
weight : 9956
draft : false
images : []
type : docs
---

Zend Framework 2 (ZF2) is a modern and flexible PHP framework that helps web developpers to build web applications of different complexities. The major sponsor of company Zend Framework is [Zend Technologies][1], which makes it very strong and stable. There are two major improvments of this second version over ZF1. First, a module-based architecture has been adopted by default without any tweak. This comes handy when developping a big sized web application that requires a decomposition to modules. Second, ZF2 implements all the features PHP5.3+ can offer particularly the namespaces. In the previous versions, a controller class is named as follows:

    class IndexController extends Zend_Controller_Action
    {

    }

This same class is rewritten in ZF2 as follows:

    namespace Application\Controller;
    use Zend\Mvc\Controller\AbstractActionController;

    class IndexController extends AbstractActionController
    {
    
    }

The following are some other exciting features of ZF2:

 - Dependency Injection
 - EventManager
 - ServiceManager


  [1]: http://www.zend.com/

