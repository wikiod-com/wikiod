---
title: "Webhooks"
slug: "webhooks"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

There are two methods to get messages and informations from your bot:

The `/getUpdates` function wich is documented pretty straight forward in the [spectific section of the documentation](https://core.telegram.org/bots/api#getupdates)

Next: The webhook function which is a bit more complex and often causes issues. 

The difference basically is that you use the first alternative to **pull** changes and react to them and the second one acts more like a **push** method, by sending a json document.

## Setup the webhook
Documented [here](https://core.telegram.org/bots/api#setwebhook) in the API you see that the syntax of the call is `https://api.telegram.org/bot<token>/setwebhook?url=<yoururl>`. For the most part this is it. If you need more information, take a look at [this guide](https://core.telegram.org/bots/webhooks) or look at the requirements (like using HTTPS).

After you set this, you can use the `/getwebhookinfo` method to get information about your webhook (JSON file). The attributes mentioned in this file are explained [in the documentation](https://core.telegram.org/bots/api#getwebhookinfo).

>Note that there are several ways to set up your webhook. If you have problems with your webhook always describe how you set up your webhook (with custom certificate/ without ...).
Furthermore if you got problems always check if you met the requirements of the webhooks and if your HTTPS certificate is valid. 

