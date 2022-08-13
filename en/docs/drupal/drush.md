---
title: "Drush"
slug: "drush"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

# What is Drush?

Drush is a command-line scripting interface for Drupal sites. It allows for command-line management of Drupal sites.

## Drush commands
**Drush status**
------------

    drush status

This will give you an overview of your Drupal site. Version, URI, database location, file paths, default theme etc. If you use this command and you do not see this information, it means you are in a wrong folder and Drush does not know which Drupal site you are referring to.

----------

**Resetting password for any user**
-----------------------------------

    drush upwd admin --password="newpassword"

Where "admin" is an existing username and "newpassword" is the desired password.

----------

**Generate a one time use admin login URL**
---------------------------------------

    drush uli
Generates a URL which can be used to login into the admin section. The URL has a one time use token. The result should look like this:

    http://example.com/user/reset/1/1469178712/MEn1QOXo3YGKAUHCknFQF0rEPJ_itkS-a6I8LJwaNYs/login

Sometimes the hostname or IP is not resolvable, and the result is a warning like this:

    default does not appear to be a resolvable hostname or IP, not starting browser.   [warning]
    You may need to use the --uri option in your command or site alias to indicate
    the correct URL of this site.
    http://default/user/reset/1/1469178629/-zFS_0u8is2N2uCKuLUdGBpJ3cZzV9am5_irsbtVAOs/login

The solution is using the "--url" parameter like the following example:

    drush uli --uri="http://example.com/"

----------

**Clearing the caches**
---------------------------------------

    drush cache-rebuild
Rebuilds the cache for Drupal 8. For drupal 7, you can use

    drush cache-clear

These commands are also available shorter with

    drush cr

or

    drush cc // optionally pass all to clear all the caches

----------

**Enable modules**
---------------------------------------

    drush pm-enable mymodule
Enable 'mymodule' like activating a module in the admin interface


These commands are also available shorter with

    drush en mymodule // optionally pass the -y option to avoid the interactive question 


----------

**Mainteneace**
---------------------------------------
To enable the maintenance mode using Drush you can use this command:

    drush vset maintenance_mode 1 // pass 0 to disable the maintenance

Remember to clear the caches after you have enabled / disabled the maintenance mode.



## Export Configuration
In Drupal 7 and lower, your configuration is probably stored using the [Features][1] module. To update a feature with changes from the databases use this command:

    drush features-update [feature-name] // e.g. drush features-update content_type_news
You can also use this shorthand:

    drush fu [feature-name]

Drupal 8 using "Configuration Management". To export your configuration with drush use this command

    drush config-export // optionally add -y to not have to verify it
You can also use the shorthand command

    drush cex -y


  [1]: https://www.drupal.org/project/features

## Install Drush
# Manual Global Installation

For OS X and Linux:

1. Bring up Terminal, Bash, or your normal shell.

2. Type the following into Terminal/Bash:
    ```
    # Download latest stable release using the code below or browse to github.com/drush-ops/drush/releases.
    php -r "readfile('http://files.drush.org/drush.phar');" > drush
    # Or use our upcoming release: php -r "readfile('http://files.drush.org/drush-unstable.phar');" > drush

    # Test your install.
    php drush core-status

    # Make `drush` executable as a command from anywhere. Destination can be anywhere on $PATH.
    chmod +x drush
    sudo mv drush /usr/local/bin

    # Optional. Enrich the bash startup file with completion and aliases.
    drush init
    ```

For Windows:

 1. Download [Drush][1] from GitHub.
 2. Extract the compressed file in your desired drive, for eg.
    ```
    C:\
    ```
 3. Install Drush, define the extracted folder path in the environment path variable which should also include the `Apache, PHP and MySQL`.
    ```
    C:\xampp\apache\bin;C:\xampp\mysql\bin;C:\xampp\php;C:\drush;
    ```
 4. Verify that Drush works:
    ```
    drush status
    ```

# Composer Global Installation

1.  [Install Composer globally][2].
2.  Add Composer's `bin` directory to the system path by placing `export PATH="$HOME/.composer/vendor/bin:$PATH"` into your ~/.bash_profile (OS X) or ~/.bashrc (Linux).
3.  Install Drush:
    ```
    composer global require drush/drush
    ```
4.  Verify that Drush works:
    ```
    drush status
    ```

More details from [Drush Docs][3]


  [1]: https://github.com/drush-ops/drush/releases/tag/6.7.0
  [2]: https://getcomposer.org/doc/00-intro.md#globally
  [3]: http://docs.drush.org/en/master/

## Install Drush
Manual installation
-------------------

Type below commands in Terminal.

<!-- language: lang-bash -->

    php -r "readfile('http://files.drush.org/drush.phar');" > drush
    chmod +x drush
    sudo mv drush /usr/local/bin
    drush init # Add alias in bash startup file.

Composer
--------

Assuming composer is installed.

<!-- language: lang-bash -->

    composer global require drush/drush:dev-master



