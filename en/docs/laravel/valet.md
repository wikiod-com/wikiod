---
title: "Valet"
slug: "valet"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

Valet is a development environment tailor made for macOS. It abstracts away the need for virtual machines, Homestead, or Vagrant. No need to constantly update your `/etc/hosts` file anymore. You can even share your sites publicly using local tunnels.

Laravel Valet makes all sites available on a `*.dev` domain by binding folder names to domain names.

## Syntax
 - valet command [options] [arguments]


## Parameters
| Parameter | Values Set |
| ------ | ------ |
| command | [domain](https://www.wikiod.com/laravel/valet#Valet domain), fetch-share-url, forget, help, install, [link](https://www.wikiod.com/laravel/valet#Valet link), [links](https://www.wikiod.com/laravel/valet#Valet links), list, logs, on-latest-version, open, [park](https://www.wikiod.com/laravel/valet#Valet park), paths, restart, secure, start, stop, uninstall, unlink, unsecure, which |
| options | -h, --help, -q, --quiet, -V, --version, --ansi, --no-ansi, -n, --no-interaction, -v, -vv, -vvv,--verbose |
| arguments | *(optional)* |

Because Valet for Linux and Windows are unofficial, there will not be support outside of their respective Github repositories.

## Valet link
This command is useful if you want to serve a single site in a directory and not the entire directory.

    cd ~/Projects/my-blog/
    valet link awesome-blog

Valet will create a symbolic link in `~/.valet/Sites` which points to your current working directory.  
After running the link command, you can access the site in your browser at `http://awesome-blog.dev`.  

To see a listing of all of your linked directories, run the [valet links][1] command. You may use `valet unlink awesome-blog` to destroy the symbolic link.


  [1]: https://www.wikiod.com/laravel/valet#Valet links

## Valet links
This command will display all the registered Valet links you have created and their corresponding file paths on your computer.

**Command:**

    valet links
    
**Sample Output:**

    ...
    site1 -> /path/to/site/one
    site2 -> /path/to/site/two
    ...


**Note 1:** You can run this command from anywhere not just from within a linked folder. 

**Note 2:** Sites will be listed without the ending **.dev** but you'll still use **site1.dev** to access your application from the browser.

## Valet park
    cd ~/Projects
    valet park

This command will register your current working directory as a path that Valet should search for sites. Now, any Laravel project you create within your "parked" directory will automatically be served using the `http://folder-name.dev` convention.

## Installation
***IMPORTANT!!* Valet is a tool designed for macOS only.**

**Prerequisites** 

- Valet utilizes your local machine's HTTP port (port 80), therefore, you will not be able to use if *Apache* or *Nginx* are installed and running on the same machine.
- macOS' unofficial package manager [Homebrew][1] is required to properly use Valet. 
- Make sure Homebrew is updated to the latest version by running `brew update` in the terminal.


**Installation**

- Install PHP 7.1 using Homebrew via `brew install homebrew/php/php71`.
- Install Valet with Composer via `composer global require laravel/valet`.
- Append `~/.composer/vendor/bin` directory to your system's "PATH" if it is not already there.
- Run the `valet install` command.

**Post Install**
During the installation process, Valet installed *DnsMasq*. It also registered Valet's daemon to automatically launch when your system starts, so you don't need to run `valet start` or `valet install` every time you reboot your machine.

  [1]: https://brew.sh

## Valet domain
This command allows you to change or view the TLD *(top-level domain)* used to bind domains to your local machine.

**Get The Current TLD**

    $ valet domain
    > dev

**Set the TLD**

    $ valet domain local
    > Your Valet domain has been updated to [local].



## Installation (Linux)
***IMPORTANT!! Valet is a tool designed for macOS, the version below is ported for Linux OS.***

**Prerequisites**

- **Do not** install valet as `root` or by using the `sudo` command.
- Valet utilizes your local machine's HTTP port (port 80), therefore, you will not be able to use if Apache or Nginx are installed and running on the same machine.
- An up to date version of `composer` is required to install and run Valet.

**Installation**

- Run `composer global require cpriego/valet-linux` to install Valet globally.
- Run the `valet install` command to finish the installation.

**Post Install** 

During the installation process, Valet installed DnsMasq. It also registered Valet's daemon to automatically launch when your system starts, so you don't need to run `valet start` or `valet install` every time you reboot your machine.

The [Official Documentation can be found here][1].


  [1]: https://github.com/cpriego/valet-linux/wiki

