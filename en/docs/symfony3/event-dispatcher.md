---
title: "Event Dispatcher"
slug: "event-dispatcher"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
- $dispatcher->dispatch(string $eventName, Event $event);
- $dispatcher->addListener(string $eventName, callable $listener, int $priority = 0);
- $dispatcher->addSubscriber(EventSubscriberInterface $subscriber);

- It is often best to use a single instance of EventDispatcher in your application that you inject into the objects that need to fire events.
- It is best practice to have a single location where you manage the configuration of, and add event listeners to, your EventDispatcher. The Symfony framework uses the Dependency Injection Container.
- These patterns will allow you to easily change your event listeners without needing to change the code of any module that is dispatching events.
- The decoupling of event dispatch from event listener configuration is what makes the Symfony EventDispatcher so powerful
- The EventDispatcher helps you satisfy the Open/Closed Principle.

## Event Dispatcher Quick Start
<!-- language: lang-php -->
    use Symfony\Component\EventDispatcher\EventDispatcher;
    use Symfony\Component\EventDispatcher\Event;
    use Symfony\Component\EventDispatcher\GenericEvent;

    // you may store this in a dependency injection container for use as a service
    $dispatcher = new EventDispatcher();

    // you can attach listeners to specific events directly with any callable
    $dispatcher->addListener('an.event.occurred', function(Event $event) {
        // process $event
    });

    // somewhere in your system, an event happens
    $data = // some important object
    $event = new GenericEvent($data, ['more' => 'event information']);

    // dispatch the event
    // our listener on "an.event.occurred" above will be called with $event
    // we could attach many more listeners to this event, and they too would be called
    $dispatcher->dispatch('an.event.occurred', $event);

    

## Event Subscribers
<!-- language: lang-php -->
    use Symfony\Component\EventDispatcher\EventDispatcher;
    use Symfony\Component\EventDispatcher\EventSubscriberInterface;
    use Symfony\Component\EventDispatcher\Event;

    $dispatcher = new EventDispatcher();

    // you can attach event subscribers, which allow a single object to subscribe 
    // to many events at once
    $dispatcher->addSubscriber(new class implements EventSubscriberInterface {
        public static function getSubscribedEvents()
        {
            // here we subscribe our class methods to listen to various events
            return [
                // when anything fires a "an.event.occurred" call "onEventOccurred"
                'an.event.occurred' => 'onEventOccurred',
                // an array of listeners subscribes multiple methods to one event
                'another.event.happened' => ['whenAnotherHappened', 'sendEmail'],
            ];
        }

        function onEventOccurred(Event $event) {
            // process $event
        }

        function whenAnotherHappened(Event $event) {
            // process $event
        }

        function sendEmail(Event $event) {
            // process $event
        }
    });

