---
title: "Signals"
slug: "signals"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Parameters
| Class/Method | The Why |
| ------ | ------ |
| UserProfile() Class   | The UserProfile class extends the [Django default User Model][1].   |
| create_profile() method | The create_profile() method is executed, whenever a Django User model [`post_save` signal is released][2]. |


  [1]: https://docs.djangoproject.com/en/1.9/topics/auth/customizing/
  [2]: https://docs.djangoproject.com/en/1.9/ref/signals/#post-save

Now, the details.

Django signals is a way to inform your app of certain tasks (such as a model pre- or post-save or delete) when it takes place. 

These signals allow you to perform actions of your choice immediately that signal is released. 

For instance, **anytime** a new Django User is created, the User Model releases a signal, with associating params such as `sender=User` allowing you to specifically target your listening of signals to a specific activity that happens, in this case, a new user creation.

In the above example, the intention is to have a UserProfile object created, *immediately* after a User object is created. Therefore, by listening to a `post_save` signal from the `User` model (the default Django User Model) specifically, we create a `UserProfile` object just after a new `User` is created.

The Django Documentation provides extensive documentation on all the possible [signals available][1].

However, the above example is to explain in practical terms a typical use case when using Signals can be a useful addition.

"With great power, comes great responsibility". It can be tempting to having signals scattered across your entire app or project just because they're awesome. Well, Don't. Because they're cool doesn't make them the go-to solution for every simple situation that comes to mind.

Signals are great for, as usual, not everything. Login/Logouts, signals are great. Key models releasing signs, like the User Model, if fine.

Creating signals for each and every model in your app can get overwhelming at a point, and defeat the whole idea of the sparring use of Django Signals.

**Do not use signals when** (based on [Two Scoops of Django book][2]):

 - The signal relates to one particular model and can be moved into one of that model’s methods, possibly called by `save()`.
 - The signal can be replaced with a custom model manager method.
 - The  signal relates to a particular view and can be moved into that view

**It might be okay to use signals when:**
 - Your signal receiver needs to make changes to more than one model.
 - You want to dispatch the same signal from multiple apps and have them handled the same way by a common receiver.
 - You want to invalidate a cache after a model save.
 - You have an unusual scenario that needs a callback, and there’s no other way to handle it besides using a signal. For example, you want to trigger something based on the `save()` or `init()` of a third-party app's model. You can't modify the third-party code and extending it might be impossible, so a signal provides a trigger for a callback.

  [1]: https://docs.djangoproject.com/en/1.9/ref/signals/#
  [2]: https://www.twoscoopspress.com/products/two-scoops-of-django-1-8

## Extending User Profile Example
This example is a snippet taken from the [Extending Django User Profile like a Pro][1]

    from django.db import models
    from django.contrib.auth.models import User
    from django.db.models.signals import post_save
     
    class UserProfile(models.Model):
        user = models.OneToOneField(User, related_name='user')
        website = models.URLField(default='', blank=True)
        bio = models.TextField(default='', blank=True)

    def create_profile(sender, **kwargs):
        user = kwargs["instance"]
        if kwargs["created"]:
            user_profile = UserProfile(user=user)
            user_profile.save()
    post_save.connect(create_profile, sender=User)


  [1]: https://blog.khophi.co/extending-django-user-model-userprofile-like-a-pro/

## Different syntax to post/pre a signal
    from django.db import models
    from django.contrib.auth.models import User
    from django.db.models.signals import post_save
    from django.dispatch import receiver
     
    class UserProfile(models.Model):
        user = models.OneToOneField(User, related_name='user')
        website = models.URLField(default='', blank=True)
        bio = models.TextField(default='', blank=True)

    @receiver(post_save, sender=UserProfile)
    def post_save_user(sender, **kwargs):
        user = kwargs.get('instance')
        if kwargs.get('created'):
            ...

## How to find if it's an insert or update in the pre_save signal
By utilizing the `pre_save` we can determine if a `save` action on our database was about updating an existing object or creating a new one.

In order to achieve this you can check the state of the model object:


        @receiver(pre_save, sender=User)
        def pre_save_user(sender, instance, **kwargs): 
            if not instance._state.adding:
                print ('this is an update')
            else:
                print ('this is an insert')

Now every time a `save` action takes place, the `pre_save` signal will run and will print:

 - `this is an update` if the action derived from an update action.
 - `this is an insert` if the action derived from an insert action.

<hr>

Note that this method does not require any additional database queries.


  [1]: https://docs.djangoproject.com/en/1.11/ref/models/instances/#customizing-model-loading

## Inheriting Signals on Extended Models
Django's signals are restricted to precise class signatures upon registration, and thus subclassed models are not immediately registered onto the same signal.

Take this model and signal for example
```
class Event(models.Model):
    user = models.ForeignKey(User)


class StatusChange(Event):
    ...


class Comment(Event):
    ...


def send_activity_notification(sender, instance: Event, raw: bool, **kwargs):
    """
    Fire a notification upon saving an event
    """

    if not raw:
        msg_factory = MessageFactory(instance.id)
        msg_factory.on_activity(str(instance))
post_save.connect(send_activity_notification, Event)
```

With extended models, you must manually attach the signal onto each subclass else they won't be effected.

```
post_save.connect(send_activity_notification, StatusChange)
post_save.connect(send_activity_notification, Comment)
```

With Python 3.6, you can leverage some additional class methods build into classes to automate this binding.

```
class Event(models.Model):

    @classmethod
    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        post_save.connect(send_activity_notification, cls)
```

