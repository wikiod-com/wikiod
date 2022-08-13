---
title: "Devise & OmniAuth Multiple Providers"
slug: "devise--omniauth-multiple-providers"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Add and install the Devise gem


## Add authentication_provider migration


## Add Associations in authentication_provider.rb


## Add the following code to user.rb


## Create Controller users/omniauth_callbacks_controller.rb


## Add the following code to user_authentication.rb


## Add Model Concern omniauth_attributes_concern.rb


## Add route for users/omniauth_callbacks_controller.rb in routes.rb


## Add Social Media Account Keys in devise.rb


## Add Gems in Gemfile for OmniAuth


## Create Controller Concern omni_concern.rb


## Add user_authentication migration


