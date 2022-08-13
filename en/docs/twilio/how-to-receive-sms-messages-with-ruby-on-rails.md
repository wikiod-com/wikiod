---
title: "How To Receive SMS Messages with Ruby on Rails"
slug: "how-to-receive-sms-messages-with-ruby-on-rails"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Using the Twilio Gem
Assuming you have a Twilio account and [API credentials](https://www.twilio.com/console/account/settings), add the following to your Gemfile:

    gem 'twilio-ruby'

Alternatively you can `gem install twilio-ruby`.

To have Twilio send an incoming SMS to a particular route in your application, you need to configure the Messaging URL for your [phone number](https://www.twilio.com/console/phone-numbers/incoming).  Once that's done, you need to set up a route in `config/routes.rb`:

```
TwilioSample::Application.routes.draw do
  resources :messages
end
```

This creates a set RESTful routes (`GET /messages` to list messages, `POST /messages` to create a message, etc.) that will send requests to the `MessagesController`.  With these routes defined, you would set the Messaging URL in the Twilio dashboard to `https://your.site/messages` as an HTTP POST Webhook.

Your controller handles the message:

```ruby
class MessagesController < ApplicationController

  def create
    # First, check to see if this message really came from Twilio
    uri, signature = [request.protocol, request.host_with_port, request.path].join, request.env['HTTP_X_TWILIO_SIGNATURE']
    validator = Twilio::Util::RequestValidator.new(ENV['TWILIO_AUTH_TOKEN'])
    
    if validator.validate(uri, request.request_parameters, signature)
      # The params hash has the data from the Twilio payload
      Rails.logger.info "I just got this message from #{params[:From]}: #{params[:Body]}"

      # Send back a response that Twilio can handle
      render xml: { Sms: "Thanks, I got your message" }.to_xml(root: 'Response')

    else
      # This request didn't come from Twilio, so return a "Forbidden" response
      head(403)
    end

  end

end




