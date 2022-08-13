---
title: "WP-CLI"
slug: "wp-cli"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

WP-CLI is a set of command-line tools for managing WordPress installations. You can update plugins, configure multisite installs and much more, without using a web browser.

## Manage themes
Get a list of themes.
    
    $ wp theme list

Install the latest version from wordpress.org and activate

    $ wp theme install twentysixteen --activate

Install from a local zip file

    $ wp theme install ../my-theme.zip

Install from a remote zip file

    $ wp theme install http://s3.amazonaws.com/bucketname/my-theme.zip?AWSAccessKeyId=123&amp;Expires=456&amp;Signature=abcdef

Get details of an installed theme

    $ wp theme get twentysixteen --fields=name,title,version

Get status of theme

    $ wp theme status twentysixteen



## Manage plugins
Get a list of plugins

    $ wp plugin list 

List active plugins on the site.

    $ wp plugin list --status=active --format=json

List plugins on each site in a network.

    $ wp site list --field=url | xargs -I % wp plugin list --url=%

Activate plugin

    $ wp plugin activate hello-dolly

Deactivate plugin

    $ wp plugin deactivate hello-dolly

Delete plugin

    $ wp plugin delete hello-dolly

Install the latest version from wordpress.org and activate

    $ wp plugin install bbpress --activate

## Manage WP-CLI itself
Display the version currently installed.

    $ wp cli version

Check for updates to WP-CLI.

    $ wp cli check-update

Update WP-CLI to the latest stable release.

    $ wp cli update

List all available aliases.

    $ wp cli alias

Print various details about the WP-CLI environment.

    $ wp cli info

Dump the list of installed commands, as JSON.

    $ wp cli cmd-dump

## Download, install, update and manage a WordPress install.
Download WordPress core

    $ wp core download --locale=nl_NL

Install WordPress

    $ wp core install --url=example.com --title=Example --admin_user=supervisor --admin_password=strongpassword --admin_email=info@example.com

Display the WordPress version

    $ wp core version

Transform a single-site install into a WordPress multisite install.

    $ wp core multisite-convert

Install WordPress multisite from scratch.

    $ wp core multisite-install 

## Manage users
List user IDs

    $ wp user list --field=ID

Create a new user.

    $ wp user create bob bob@example.com --role=author

Update an existing user.

    $ wp user update 123 --display_name=Mary --user_pass=marypass

Delete user 123 and reassign posts to user 567

    $ wp user delete 123 --reassign=567

## Perform basic database operations using credentials stored in wp-config.php
Create a new database.

    $ wp db create

Drop an existing database.

    $ wp db drop --yes

Reset the current database.

    $ wp db reset --yes

Execute a SQL query stored in a file.

    $ wp db query < debug.sql

