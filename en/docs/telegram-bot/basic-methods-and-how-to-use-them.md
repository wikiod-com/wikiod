---
title: "Basic methods and how to use them"
slug: "basic-methods-and-how-to-use-them"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Different methods and their arguments are used to achieve the wished behaviour of your telegram bot, which you created by now (hopefully).

The basic structure of a query is (as mentioned in "Create a bot with the BotFather"):
```https://api.telegram.org/bot*BOTTOKEN*/*METHOD*?*ARGUMENT1*=*VALUE1*&*ARGUMENT2*=*VALUE2*```
You will use this structure with as many arguments you need/ want and this documentation will show you the basic methods and how to use them.

## SendMessage method
```https://api.telegram.org/bot*BOTTOKEN*/sendmessage?chat_id=exampleID&text=exampleText&parse_mode=HTML```

What does this do? With your parameters adjusted correctly this call will send a message to the **exampleID** user with the **exampleText** as message with an HTML encoding. Standard encoding is markdown (see [source][1]) but in some cases you may use HTML. 

Next to the *chat_id*, *text* and *parse_mode* you can use the following parameters: 
 - *disable_web_page_preview*    - For disabling the standard preview if you send a link
 - *disable_notification*    - For disabling the notification on userside (Android users will still get a notification, but without sound)
 - *reply_to_message_id*    - Sends the message as a reply to another message (with this ID)
 - *reply_markup*    - Send your custom keyboard with this parameter

Only the *chat_id* and the *text* parameter are required to send a simple message, all other operators are optional. The *disable_web_page_preview* and the *disable_notification* method will need a boolean operator (true or false) to work. All of the parameters are case sensitive so watch out!

For more information jump to the [sendMessage part of the bot api doku][2].


  [1]: https://core.telegram.org/bots/api#formatting-options
  [2]: https://core.telegram.org/bots/api#sendmessage

