---
title: "Events Manager"
slug: "events-manager"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Dynamic ACL check
Create a Security class to run your ACL logic.
```
<?php

namespace Plugins;

use Phalcon\Events\Event;
use Phalcon\Mvc\Dispatcher;
use Phalcon\Acl;
use Phalcon\Acl\Role;
use Phalcon\Acl\Resource;
use Phalcon\Acl\Adapter\Memory as AclList;

class Security extends \Phalcon\Mvc\User\Plugin
{
    public function beforeExecuteRoute(Event $event, Dispatcher $dispatcher)
    {
        // your acl logic here
    }
}
```
Hook the Security class to the dispatcher, to run on *beforeExecuteRoute*.
```
$di = new \Phalcon\DI\FactoryDefault();
$eventsManager = $di['eventsManager'];

$di->setShared('dispatcher', function() use ($eventsManager) {
    $eventsManager->attach('dispatch:beforeExecuteRoute', new \Plugins\Security);
    $dispatcher = new \Phalcon\Mvc\Dispatcher;
    $dispatcher->setEventsManager($eventsManager);
    return $dispatcher;
});
```

