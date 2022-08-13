---
title: "Access Manager"
slug: "access-manager"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## pubnub access manager at server side
PubNub Access Manager (PAM) extends PubNub's existing security framework by allowing developers to create and enforce secure access to channels throughout the PubNub Real Time Network.

Access Manager allows you to manage granular permissions for your realtime apps and data streams, create multiple permission levels, grant and revoke access, and audit user access.

To use Access Manager, you need to enable Access Manager in the Admin Dashboard.  Once you enable Access Manager, you must grant permissions before any data can be sent or received.

    PAM Server side Configuration

In order to client side working correctly, at server side must first issue the appropriate permissions for a given PAM channel or channel-group and auth token combination. 

for granting these permission you must initialize pubnub instance at least with your  subscribe and secret keys.

 Example : 

Step 1. Make Pubnub Configuration : -

    PNConfiguration pnConfiguration = new PNConfiguration();
            pnConfiguration.setSubscribeKey(SUBSCRIBE_KEY);
            pnConfiguration.setPublishKey(PUBLISH_KEY);;
            pnConfiguration.setSecretKey(SECRET_KEY);
            pnConfiguration.setSecure(true);
            pnConfiguration.setLogVerbosity(PNLogVerbosity.BODY);
        

Step 2. Initialize PubNub with pnConfiguration

    PubNub pubnub = new PubNub(pnConfiguration);


PAM Operation occurs at three level

    1. A global level (no auth key, and no channel/channel group is defined) 

    2. A channel/channel group level (only a channel/channel group is defined) 

    3. A channel/channel group and key level (where both the channel/channel     group and key are defined)


At all these levels we can grant , revoke and audit permissions. Here we do the same on channel/channel group and auth key level.

    PAM  Grant

we can grant a read/write permission to auth_key on  specific channels or channel groups

Example:

Synchronously: 

    try {
    
        pubnub.grant().authKeys(Arrays.asList("auth1,auth2"))
        .channels(Arrays.asList("channel1,channel2")).read(true).write(true    ).ttl(0).sync();
    
    } catch (PubNubException e) {
        e.printStackTrace();
    }


Asynchronously:

pubNub.grant().channels(channels).authKeys(Arrays.asList(authKey)).read(true).write(true).manage(false).ttl(0)
.async(new PNCallback<PNAccessManagerGrantResult>() {

    @Override
    public void onResponse(PNAccessManagerGrantResult result,
         PNStatus status) {
    }});

 
`PAM REVOKE:` we can revoke a permission to auth_key from a specific channel or channel groups.

Syntax for revoking permission same as granting . Just we need to change the permission true to false.

    try {
    
        pubnub.grant().authKeys(Arrays.asList("auth1,auth2"))
        .channels(Arrays.asList("channel1,channel2")).read(false).write( false    ).ttl(0).sync();
    
    } catch (PubNubException e) {
        e.printStackTrace();
    }


`PAM Audit:` we can audit a given permission to specific  channel/channel group or to a given auth_key on specific channel or channel group

Example:

    pubnub.audit().channel("mycha").authKeys(Arrays.asList("a1")).async(new PNCallback<PNAccessManagerAuditResult>(){
    
        @Override
        public void onResponse(PNAccessManagerAuditResult result,
                PNStatus status) {
                    
                }
        });   

`PAM Add Channels into groups:` we can also add channels into channel groups 

Example:


    pubnub.addChannelsToChannelGroup().channelGroup("my_channel").channels(Arrays.asList("my_channel5"))
        .async(new PNCallback<PNChannelGroupsAddChannelResult>() {
    
                @Override
                public void onResponse(PNChannelGroupsAddChannelResult                                                                                                                                             result,PNStatus status) {
                    
                    
                }
    });



`Authentication Isue at Client Side` `(403 Forbidden):`

 If there is an error performing PAM operations, you may receive a 403 error. If you do, be sure you have set the correct secret_key, and the issuing computer's clock is synced with NTP.


    NTP  Setup

Network Time Protocol (NTP) is a protocol that is used to synchronize computer clock times in a network of computers. NTP uses Coordinated Universal Time (UTC) to synchronize computer clock times to a millisecond, and sometimes to a fraction of a millisecond.

Here we need to scyn server time with pubnub. Follow the step for doing so

Step 1 Intallation NTP

    $ sudo apt-get update
    $ sudo apt-get install ntp

Step 2 Edit ntp.conf

    Replace these four with pubnub server 

    server 0.ubuntu.pool.ntp.org 
    server 1.ubuntu.pool.ntp.org 
    server 2.ubuntu.pool.ntp.org 
    server 3.ubuntu.pool.ntp.org

   

 to

    server 0.pubsub.pubnub.com
    server 1.pubsub.pubnub.com
    server 2.pubsub.pubnub.com
    server 3.pubsub.pubnub.com

Step 3 Restart NTP Service

$ sudo service ntp restart


Ref : 

[https://www.pubnub.com/docs/web-javascript/pam-security][1]

https://www.pubnub.com/docs/java/pubnub-java-sdk-v4


  [1]: https://www.pubnub.com/docs/web-javascript/pam-security



## Wildcard Channel Group Manage Grant - Java SDK v4
When it comes to adding/removing channels to/from your channel groups, you need to have must have the `manage` permission for those channel groups. But you should never grant clients the permission to `manage` the channel groups that they will subscribe to. If they did, then they could add any channel they wanted to their channel group and instantly have read access to that channel. 

So this is why your server must be the only entity that has the `manage` permission. But your server will need to have the `manage` permission for every single channel group so that it can add/remove channels to/from channel groups on behalf of all of the clients. 

But granting `manage` to each and every channel group can be a bit tedious. Instead, you can grant `manage` to all channel groups (existing and to be created) in one *wildcard* grant.

<!-- language: lang-java -->

    // init PubNub instance using PNConfiguration with the secret-key
    PNConfiguration pnConfiguration = new PNConfiguration();
    pnConfiguration.setSubscribeKey("my_subkey")
    pnConfiguration.setPublishKey("my_pubkey");
    // secret key allows server to `grant` permissions
    pnConfiguration.setSecretKey("my_secretkey");
    pnConfiguration.setSecure(true);
    // set the the server's auth key
    pnConfiguration.setAuthKey("server_authkey");
    PubNub pubnub = new PubNub(pnConfiguration);
    
    // grant read and manage using the channel group wildcard - ":" 
    // with forever ttl (0) 
    pubNub.grant()
        .channelGroups(Arrays.asList(":")) // colon (:) is channel group wildcard
        .manage(true) // add/remove channels to/from channel groups
        .read(true) // in case server needs to subscribe or do here-now on channel groups
        .ttl(0) // 0 = forever grant
        .async(new PNCallback<PNAccessManagerGrantResult>() {
            @Override
            public void onResponse(PNAccessManagerGrantResult result, PNStatus status) {
                // check status for success or failure of grant
            }
        });

From here on, your server will be able to add/remove channels to/from any channel group your app creates.

