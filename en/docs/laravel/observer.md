---
title: "Observer"
slug: "observer"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Creating an observer
Observers are used for listening to livecycle callbacks of a certain model in Laravel.  
These listeners may listen to any of the following actions:
- creating
- created
- updating
- updated
- saving
- saved
- deleting
- deleted
- restoring
- restored

Here is an example of an observer.

**UserObserver**

<!-- language-all: php -->

```
<?php

namespace App\Observers;

/**
 * Observes the Users model
 */
class UserObserver 
{
    /**
     * Function will be triggerd when a user is updated
     *
     * @param Users $model
     */
     public function updated($model)
     {
         // execute your own code
     }
}
```

As shown in the user observer, we listen to the updated action, however before this class actually listens to the user model we first need to register it inside the `EventServiceProvider`.

**EventServiceProvider**

    <?php 
    
    namespace App\Providers;

    use Illuminate\Contracts\Events\Dispatcher as DispatcherContract;
    use Illuminate\Foundation\Support\Providers\EventServiceProvider as ServiceProvider;

    use App\Models\Users;
    use App\Observers\UserObserver;

    /**
     * Event service provider class
     */
    class EventServiceProvider extends ServiceProvider
    {
        /**
         * Boot function
         *
         * @param DispatcherContract $events
         */
        public function boot(DispatcherContract $events)
        {
            parent::boot($events);

            // In this case we have a User model that we want to observe
            // We tell Laravel that the observer for the user model is the UserObserver
            Users::observe(new UserObserver());
        }
    }
    
Now that we have registered our observer, the updated function will be called every time after saving the user model.


