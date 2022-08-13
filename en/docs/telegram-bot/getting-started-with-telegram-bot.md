---
title: "Getting started with telegram-bot"
slug: "getting-started-with-telegram-bot"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create a bot with the BotFather
Open a chat with [@BotFather][1] in Telegram and click the `/start` command. 

After you get a list of commands Select the command `/newbot` to get this Message:

> Alright, a new bot. How are we going to call it? Please choose a name
> for your bot.

Enter a name for your bot, which can be anything, and send it. After that BotFather will ask for a username for your bot:

> Good. Now let's choose a username for your bot. It must end in `bot`.
> Like this, for example: TetrisBot or tetris_bot.

That must end with “bot”. If your desired username is taken or not available, try again with another name.

Finally your bot is created and the following message is shown. You must use the bot token to communicate with the Telegram, so make sure you write it down.
> Done! Congratulations on your new bot. You will find it at
> telegram.me/???bot. You can now add a description, about section and
> profile picture for your bot, see /help for a list of commands. By the
> way, when you've finished creating your cool bot, ping our Bot Support
> if you want a better username for it. Just make sure the bot is fully
> operational before you do this.
> 
> Use this token to access the HTTP API: xxx:xxx
> 
With the gained token you now can send a test message by calling the website ```https://api.telegram.org/botBOTTOKEN/sendmessage?chat_id=YOURCHATID&text=YOURTEXT```

There are two general methods for the telegram bots for interaction: a push and a pull method. Using the pull method you have to call the ```/getupdates``` every once in a while to check if there are new messages send to your bot. The push method uses a webhook (```/setwebhook``` method) to your script which is called every time a user sends a message to your bot.

The provided information is a [JSON formatted file][2] with all the info (of the sender and the message) you need. 

You can write your bot in many languages (PHP, Python, Lua...) as long as they can handle the webhook as an input and can call websites.

For more info you can always use the BOT API documentation which you can find [here.][3]


  [1]: https://telegram.me/BotFather
  [2]: https://core.telegram.org/bots/api#update
  [3]: https://core.telegram.org/bots/api

