---
title: "UUIDs"
slug: "uuids"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## JavaScript/Web SDK
For JavaScript, here is the code we recommend for generating, persisting and retrieving a UUID. This could be wrapped in a function can called directly from the PUBNUB.init function rather than the two step inline solution below.

    // get/create/store UUID
    var UUID = PUBNUB.db.get('session') || (function(){ 
        var uuid = PUBNUB.uuid(); 
        PUBNUB.db.set('session', uuid); 
        return uuid; 
    })();
    
    // init PUBNUB object with UUID value
    var pubnub = PUBNUB.init({
        publish_key: pubKey,
        subscribe_key: subKey,
        uuid: UUID
    });

## Android/Java SDK
For Android, here is the code we recommend for generating, persisting and retrieving a UUID. There is not constructor that accepts the UUID as a parameter, so you must instantiate `Pubnub` object first then use the setter to provide the UUID.

    // creating the Pubnub connection object with minimal args
    Pubnub pubnub = new Pubnub(pubKey, subKey);
    
    // get the SharedPreferences object using private mode 
    // so that this uuid is only used/updated by this app 
    SharedPreferences sharedPrefs = getActivity().getPreferences(Context.MODE_PRIVATE);
    
    // get the current pn_uuid value (first time, it will be null)
    String uuid = getResources().getString(R.string.pn_uuid);
    
    // if uuid hasn’t been created & persisted, then create
    // and persist to use for subsequent app loads/connections 
    if (uuid == null || uuid.length == 0) {
        // PubNub provides a uuid generator method but you could 
        // use your own custom uuid, if required
        uuid = pubnub.uuid();
        SharedPreferences.Editor editor = sharedPrefs.edit();
        editor.putString(getString(R.string.pn_uuid), uuid); 
        editor.commit();
    }
    
    // set the uuid for the pubnub object
    pubnub.setUUID(uuid);

