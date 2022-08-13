---
title: "Message Filtering"
slug: "message-filtering"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

[Stream Filter][1] provides the ability to filter messages on the server before they are sent to a subscriber is a popular request. With the introduction of our v4.x SDKs, you now have the ability to do so using message *meta data*.


  [1]: https://www.pubnub.com/docs/ios-objective-c/stream-filtering-tutorial-sdk-v4

## Prevent Receiving Your Own Messages Using Objective-C
Setting a filter applies to all channels that you will subscribe to from that particular client. This client filter *excludes* messages that have this subscriber's UUID set at the sender's UUID:

<!-- language: lang-cpp -->

    NSString *expression = [NSString stringWithFormat:@"(uuid != '%@'", 
                            self.client.currentConfiguration.uuid];
    [self.client setFilterExpression:expression];

When publishing messages, you need to include the sender's UUID if you want the subscriber side client filter to work:

<!-- language: lang-cpp -->

    [self.client publish:@"message" toChannel:@"group-chat" 
            withMetadata:@{@"uuid": self.client.currentConfiguration.uuid} 
              completion:^(PNPublishStatus *status) {

        // Check whether request successfully completed or not.
        if (!status.isError) {    
 
            // Message successfully published to specified channel.
        }
        else {

            // Request processing failed. Handle message publish error. 
            // Check 'category' property to find out possible issue 
            // publish can be attempted again using: [status retry];
        }
    }];

See also:

 - [PubNub Objective-C SDK Stream Filter Documentation][1]


  [1]: https://www.pubnub.com/docs/ios-objective-c/stream-filtering-tutorial-sdk-v4

