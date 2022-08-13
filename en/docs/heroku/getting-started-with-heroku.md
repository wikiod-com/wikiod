---
title: "Getting started with heroku"
slug: "getting-started-with-heroku"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
To create and manage Heroku apps locally you'll need the Heroku Toolbelt, here are some ways to get it.

# Download

Download the [Heroku Toolbelt][1] installer from Heroku's website.

# Homebrew

Install `heroku` with `brew`:

    brew install heroku

# Debian/Ubuntu
Run this script:

    wget -O- https://toolbelt.heroku.com/install-ubuntu.sh | sh

This script adds the Heroku repository to apt, installs the Heroku release key, installs the Heroku Toolbelt and then installs Ruby if you need it.

As with any script you find online and pipe directly to bash we highly recommend you read [the source][3] first.


  [1]: https://toolbelt.heroku.com/
  [2]: http://heroku.com
  [3]: https://toolbelt.heroku.com/install-ubuntu.sh

## Creating Heroku Applications
You can use the `heroku create` command to create a Heroku application. Each application you deploy to Heroku has its own code base, environment variables, addons, etc.

Each Heroku application has a globally unique name. If you try to create a Heroku application whose name is already taken, you will get an error.

Here's how you can create a new Heroku application:

    heroku create [app_name]

If you don't specify an application name when running `heroku create`, Heroku will create a random application name for you.

You can also specify the Amazon region in which your Heroku application should be created. By default, all Heroku applications are created in the `us` region. If you'd like to change the region, you can do so by creating the application like so:

    heroku create [app_name] --region eu

Right now, there are only two public regions: `us`, and `eu` (Europe).


## Using the Heroku Toolbelt
# Create an application
    heroku create your-app-name
# Deploy to Heroku
    git push heroku master
# Open your application in a browser
    heroku open your-app-name
# List Heroku commands
    heroku commands
# General help
    heroku help
# Help for a specific command
    heroku help <command>

