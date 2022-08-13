---
title: "Getting started with sinatra"
slug: "getting-started-with-sinatra"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Your first Sinatra app
    # app.rb
    require 'sinatra'
    
    get '/' do
        'Hello, Universe!'
    end

Install Sinatra:

    gem install sinatra


Run the app:

    ruby app.rb

That's it! Access your app at http://localhost:4567

## Installation
You can install Sinatra as a global gem:  

    gem install sinatra

or add it to a project's Gemfile

    # in Gemfile:
    gem 'sinatra'

and run `bundle install`.


