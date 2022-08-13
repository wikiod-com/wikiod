---
title: "Getting started with rubygems"
slug: "getting-started-with-rubygems"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
If you don't have any *RubyGems* installed, there is still the pre-gem approach to getting software, doing it manually:

 - Download from [RubyGems][1]
 - Unpack into a directory and `cd` there
 - Install with: `ruby setup.rb` *(you may need admin/root privilege)*

       sudo ruby setup.rb

For more details and other options, see:

    ruby setup.rb --help

[1]: https://rubygems.org/pages/download/

## Installation on Linux
Make sure you have ruby installed before installing rubygems, and then:

**Installing RubyGems Using apt-get on Ubuntu**

    sudo apt-get install rubygems

**Installing RubyGems Using yum**

    sudo yum install rubygems

**Manual Installation Method**

    wget https://rubygems.org/rubygems/rubygems-2.6.6.tgz
    tar xvf rubygems-2.6.6.tgz
    cd rubygems-2.6.6
    sudo ruby setup.rb

## Install specific precompiled ruby version and ruby gems (Ubuntu)
`sudo apt-add-repository ppa:brightbox/ruby-ng`

Hit `Enter` to confirm

`sudo apt-get update`

Then you can install your ruby version of choice (the ppa supports `ruby2.0` `ruby2.1` `ruby2.2` `ruby2.3` and legacy versions `ruby1.8` `ruby1.9.1`) Don't forget to include the respective `-dev` package for your version. Otherwise the development of native extensions such as `Nokogiri` or `mysql2` will fail.

`sudo apt-get install ruby2.3 ruby2.3-dev ruby-switch`

`sudo ruby-switch set ruby2.3`

`ruby -v`

`>> ruby 2.3.1p112 (2016-04-26) [x86_64-linux-gnu]`

Now you can install any desired gem systemwide for all users via `sudo gem install gemname`.

Note: 
- This method installs and sets ruby and rubygems system-wide for all users
- Requires sudo to install gems (`sudo gem install rails`) but don't run `bundle` as root. If it needs to install gems it will ask for the sudo password.

## Change gem source
    # list gem sources:
    gem sources -l
    # remove default gem source:
    gem sources -r https://rubygems.org/
    # add other gem sources:
    gem sources -a https://ruby.taobao.org/

