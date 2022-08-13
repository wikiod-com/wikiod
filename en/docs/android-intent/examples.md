---
title: "Examples"
slug: "examples"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Email Share using android intent [ Text Only ]
This will trigger the native email client for sharing text. 

**Parameters** : Email To address, Subject, Body.

**Code Sample :** 

you can call the function wherever you need, *(mostly inside click listeners)* like below

**Calling function**

    shareEmail("sample@gmail.com", "Email sharing example", "This the sample demo to share the sample text through native email clients using Android Intent");

**Global Function**

    public void shareEmail(String to_email_id, String subject, String body) {
        // This function will open the email client installed in the device to share from your own app through intent.
        Intent sharingIntent = new Intent(Intent.ACTION_SEND, Uri.parse("mailto:"));
        sharingIntent.setType("message/rfc822");
    
        /* All the below fields are optional. If not given simply opens the email client */
        // To email id
        sharingIntent.putExtra(Intent.EXTRA_EMAIL, new String[]{to_email_id});
        // Subject that needs to appear while sharing
        sharingIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
        // Body of the mail content shared.
        sharingIntent.putExtra(Intent.EXTRA_TEXT, body);
    
        (mContext).startActivity(Intent.createChooser(sharingIntent, "Share content through email")
    
        );
      } // shareEmail



