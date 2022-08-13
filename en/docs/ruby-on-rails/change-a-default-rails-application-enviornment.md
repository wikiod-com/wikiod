---
title: "Change a default Rails application enviornment"
slug: "change-a-default-rails-application-enviornment"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This will discuss how to change the environment so when someone types `rails s` they boot in not development but in the environment they want.

## Running on a local machine
Normally when rails environment is run by typing. This just runs the default environment which is usually `development` 

    rails s

The specific environment can be selected by using the flag `-e` for example:

    rails s -e test

Which will run the test environment.

The default environment can be changed in terminal by editing the `~/.bashrc` file, and adding the following line:

    export RAILS_ENV=production in your 



## Running on a server
If running on a remote server that is using Passenger change apache.conf to to the environment you want to use. For example this case you see `RailsEnv production`.

    <VirtualHost *:80>
      ServerName application_name.rails.local
      DocumentRoot "/Users/rails/application_name/public"
      RailsEnv production ## This is the default
    </VirtualHost>

