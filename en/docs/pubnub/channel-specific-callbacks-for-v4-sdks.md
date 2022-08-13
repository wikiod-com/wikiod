---
title: "Channel Specific Callbacks for v4 SDKs"
slug: "channel-specific-callbacks-for-v4-sdks"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## JavaScript v4 SDK Channel Specific Callbacks
In [PubNub JavaScript v3][1], you could implement a unique callback for every channel that you subscribed to as long as you called the subscribe function for each channel and implemented the callback in that subscribe like this:

<!-- language: lang-js -->

    var pubnub = new PubNub({
        publishKey: "your-pub-key",
        subscribeKey: "your-sub-key"
    });

    pubnub.subscribe({
        channel: 'ch1',
        message: function (m) {
            console.log(m + " ch1 callback");
        }
    });

    pubnub.subscribe({
        channel: 'ch2',
        message: function (m) {
            console.log(m + " ch2 callback: ");
        }
    });

So in the above code, if you publish the message `"hello"` to `ch1`:

<!-- language: lang-js -->

    publish({channel: "ch1", message: "hello"});

...then you would get the output, `hello ch1 callback`. And of course if you published the same message to `ch2`, you would get the output `hello ch2 callback`.

The ability to provide a custom callback for each channel, or a group of channels, is useful and often more desirable than creating a monolithic callback with a long `if-then-else` of channel names and code to be executed for each condition. And there are better practices to use than the above simple example but I wanted to make it easy to compare and contrast with how [PubNub JavaScript SDK v4][2] (and any v4 PubNub SDK) is designed.

PubNub v4 SDKs use a single listener to receive all `message`, `presence` and `status` events. This means that when you `subscribe` to a channel, you only provide the channel name(s) rather than a companion `message callback` function. In [JavaScript SDK v4][2], the listener looks like this:

<!-- language: lang-js -->

    pubnub.addListener({
        message: function(m) {
            console.log(JSON.stringify(m));
        },
        presence: function(p) {
            console.log(JSON.stringify(p));
        },
        status: function(s) {
            console.log(JSON.stringify(s));
        }
    });

I know many developers will be wondering how to migrate the `subscribe` code with unique `message callbacks` from SDK v3 with this sort of design without resorting to the never ending channel name conditional code that I mentioned above, like this:

<!-- language: lang-js -->

    pubnub.addListener({
        message: function(m) {
            if (m.subscribedChannel == 'ch1') {
                console.log(m.message + "ch1 callback");
            }
            else if (m.subscribedChannel == 'ch2') {
                console.log(m.message + "ch2 callback");
            }
            else {
                console.log(m.message + "default callback");
            }
        }
        // removed the other two callbacks for brevity purposes
    });

The parameter `m` that is passed into the message listener above has the following structure which is a different design than the multiple parameter design of JavaScript SDK v3.

<!-- language: lang-js -->

    {
        "channel": "ch1",
        "subscription": <undefined>,
        "timetoken": "14721821326909151",
        "message": "hello"
    }

That `timetoken` is the actual `publish timetoken`. For experienced PubNub developers, you should be excited to see that this is now available to the subscriber, but let's not get into why this is important and powerful right now.

Now I wouldn't expect any experienced JavaScript developer to write code as represented above and many advance developers might already know what to do. But for those developers that are at the beginner to intermediate level with JavaScript, the solution may not be immediately obvious. However, I know once you see this simple design approach below, it will open your eyes to the unlimited possibilities of the JavaScript language - here we go.

Let's restate the requirement:

> For every channel I subscribe to, I want to be able to provide a
> unique function to in invoke when a message is received on that
> channel. And I want to avoid the monolithic conditional channel name approach.

So what we need to do first is create a function lookup table. A `hashtable` to be exact. This table will have channel names as keys and functions (the code to invoke when a message is received on that channel) as values. If you are somewhat new to JavaScript or have been coding with the language for awhile but haven't really dove into the language features yet, this might sound odd and impossible, but it's really how JavaScript works and you've been doing it all along and didn't really know it. Let's define our function lookup table:

<!-- language: lang-js -->

    ftbl = {};

That's it - you have an object that will hold your channels and functions. Pretty simple, right? But how do you add the channels and functions? Just like any other key/value.

<!-- language: lang-js -->

    ftbl.ch1 = function(m){console.log(m.message + " ch1 callback")};
    ftbl.ch2 = function(m){console.log(m.message + " ch1 callback")};

...and so on with each channel and function you want to define. And you don't have to create all of your channel/function keys in one spot of your code. You can add each channel/function to the `ftbl` as you subscribe to a channel.

<!-- language: lang-js -->

    ftbl.ch10 = function(m){console.log(m.message + " ch10 callback")};
    pubnub.pubnub.subscribe({
        channels: ['ch10'] 
    });

OK, that's simple enough, and you can get fancier and more advanced with how you do this but just keeping it basic. But how do you invoke this function for the channel it is linked to? This is why JavaScript is so cool, powerful and easy especially if you come from a rigid and structured language like Java - check it out.

<!-- language: lang-js -->
    
    pubnub.addListener({
        message: function(m) {
            // use the channel name to get the function
            // from ftbl and invoke it 
            ftbl[m.subscribedChannel](m);
        },
        presence: function(p) {
            console.log(JSON.stringify(p));
        },
        status: function(statusEvent) {
            console.log(JSON.stringify(s));
        }
    });


That's all there is to it. Just get the function from `ftbl` using the channel name that is passed into the listener's `message` callback function and add `(m)` to the end of it and boom, it runs your function.

If the channel is `ch10`, `ftbl[m.subscribedChannel](m)` just invokes `function(m){console.log(m.message + "ch10 callback")}` passing in the `m` parameter which your function can parse and exploit as it needs to. 

So calling the following `publish` function:

<!-- language: lang-js -->

    pubnub.publish(
        {
            channel : "ch10",
            message : "hello"
        }, 
        function(status, response) {
            console.log(status, response);
        }
    );

...will result in the following message getting displayed: `hello ch10 callback`. And the equivalent for publishing to other channels that you have defined in your function lookup table. Don't forget to provide a default for unknown channels.

And don't forget the `presence` and `status` callbacks in the listener. This could be just two more function lookup tables or just a slightly more complex `ftbl`:

<!-- language: lang-js -->

    ftbl.message.ch1 = function(m){console.log(m.message + " ch1 message cb")};
    ftbl.presence.ch1 = function(m){console.log(m.message + " ch1 presence cb")};
    ftbl.status.ch1 = function(m){console.log(m.message + " ch1 status cb")};

or 

<!-- language: lang-js -->

    ftbl.ch1.message = ...
    ftbl.ch1.presence = ...
    ftbl.ch1.status = ...

I like the former better than the latter but it's really up to you. And you probably want some generic `status` event handling code anyway but it will depend on your specific requirements.

And this can get even more complicated and robust with optional functions per channel that can be invoked depending on some additional data that you embed in the message payload.

So there you go, unique callbacks for each channel. No more excuses for not wanting to migrate from 3x to 4x. But if you do have some doubts about migrating, don't hesitate to reach out to [PubNub Support][3] and we'll get you moving forward. And don't forget to review the [PubNub JavaScript SDK v3 to v4 Migration Guide][4].


  [1]: https://www.pubnub.com/docs/web-javascript/pubnub-javascript-sdk
  [2]: https://www.pubnub.com/docs/javascript/pubnub-javascript-sdk-v4
  [3]: https://pubnub.com/support
  [4]: https://www.pubnub.com/docs/web-javascript/migration-guide-from-v3

