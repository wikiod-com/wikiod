---
title: "How To Send an SMS with Ruby"
slug: "how-to-send-an-sms-with-ruby"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Using the Twilio Gem
This assumes you have a twilio account and have purchased/reserved a phone number...

If you are using bundler add the following line to your Gemfile to include `twilio-ruby` in your project:
```ruby
gem 'twilio-ruby'
```
otherwise enter `gem install twilio-ruby` on the command line. You might need `sudo` if you're using system Ruby and not ruby-env etc.

and the code
```ruby
# Instantiate a Twilio REST client with your account SID and auth token,
# which can be found on your dashboard.

client = Twilio::REST::Client.new("your_account_sid", "your_auth_token")

client.messages.create(from: "+15556667777", to: "+15558883333", body: "Hello, world!")
```


