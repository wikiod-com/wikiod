---
title: "Gem Usage"
slug: "gem-usage"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

## Installing ruby gems
This guide assumes you already have Ruby installed.
If you're using Ruby < `1.9` you'll have to manually [install RubyGems][1] as it won't be [included natively][2].

To install a ruby gem, enter the command:

    gem install [gemname]

If you are working on a project with a list of gem dependencies, then these will be listed in a file named `Gemfile`. To install a new gem in the project, add the following line of code in the `Gemfile`:

    gem 'gemname'

This `Gemfile` is used by the [Bundler gem][3] to install dependencies your project requires, this does however mean that you'll have to install Bundler first by running (if you haven't already):

    gem install bundler

Save the file, and then run the command:

    bundle install

# Specifying versions

The version number can be specified on the command live, with the `-v` flag, such as:

    gem install gemname -v 3.14

When specifying version numbers in a `Gemfile`, you have several options available:

 - No version specified (`gem 'gemname')` -- Will install the *latest* version which is compatible with other gems in the `Gemfile`.
 - Exact version specified (`gem 'gemname', '3.14'`) -- Will only attempt to install version `3.14` (and fail if this is incompatible with other gems in the `Gemfile`).
 - **Optimistic** minimum version number (`gem 'gemname', '>=3.14'`) -- Will only attempt to install the *latest* version which is compatible with other gems in the `Gemfile`, and fails if no version greater than or equal to `3.14` is compatible. The operator `>` can also be used.
 - **Pessimistic** minimum version number (`gem 'gemname', '~>3.14'`) -- This is functionally equivalent to using `gem 'gemname', '>=3.14', '<4'`. In other words, only the number after the *final period* is permitted to increase.


----

**As a best practice**: You might want to use one of the Ruby version management libraries like [rbenv](https://github.com/rbenv/rbenv) or [rvm](https://github.com/rvm/rvm). Through these libraries, you can install different versions of Ruby runtimes and gems accordingly. So, when working in a project, this will be especially handy because most of the projects are coded against a known Ruby version.


  [1]: https://rubygems.org/pages/download
  [2]: http://guides.rubygems.org/rubygems-basics/
  [3]: https://rubygems.org/gems/bundler

## Gem installation from github/filesystem
You can install a gem from github or filesystem. If the gem has been checked out from git or somehow already on the file system, you could install it using

    gem install --local path_to_gem/filename.gem

Installing gem from github. Download the sources from github

    mkdir newgem
    cd newgem
    git clone https://urltogem.git

Build the gem

    gem build GEMNAME.gemspec
    gem install gemname-version.gem



## Checking if a required gem is installed from within code
To check if a required gem is installed, from within your code, you can use the following (using nokogiri as an example):

    begin
      found_gem = Gem::Specification.find_by_name('nokogiri')
      require 'nokogiri'
      ....
      <the rest of your code>
    rescue Gem::LoadError
    end
  
However, this can be further extended to a function that can be used in setting up functionality within your code.

    def gem_installed?(gem_name)
      found_gem = false
      begin
        found_gem = Gem::Specification.find_by_name(gem_name)
      rescue Gem::LoadError
         return false
      else
        return true
      end
    end

Now you can check if the required gem is installed, and print an error message.

    if gem_installed?('nokogiri')
      require 'nokogiri'
    else
      printf "nokogiri gem required\n"
      exit 1
    end

or
     
    if gem_installed?('nokogiri')
      require 'nokogiri'
    else
      require 'REXML'
    end

## Using a Gemfile and Bundler
A `Gemfile` is the standard way to organize dependencies in your application. A basic Gemfile will look like this:

```ruby
source 'https://rubygems.org'

gem 'rack'
gem 'sinatra'
gem 'uglifier'
```

You can specify the versions of the gem you want as follows:
```ruby
# Match except on point release. Use only 1.5.X
gem 'rack', '~>1.5.2'
# Use a specific version.
gem 'sinatra', '1.4.7'
# Use at least a version or anything greater.
gem 'uglifier', '>= 1.3.0'
```

You can also pull gems straight from a git repo:

```ruby
# pull a gem from github
gem 'sinatra', git: 'https://github.com/sinatra/sinatra.git'
# you can specify a sha
gem 'sinatra', git: 'https://github.com/sinatra/sinatra.git', sha: '30d4fb468fd1d6373f82127d845b153f17b54c51'
# you can also specify a branch, though this is often unsafe
gem 'sinatra', git: 'https://github.com/sinatra/sinatra.git', branch: 'master'

```

You can also group gems depending on what they are used for. For example:

```ruby
group :development, :test do
    # This gem is only available in dev and test, not production.
    gem 'byebug'
end
```

You can specify which platform certain gems should run on if you application needs to be able to run on multiple platforms. For example:

```ruby
platform :jruby do
  gem 'activerecord-jdbc-adapter'
  gem 'jdbc-postgres'
end

platform :ruby do
  gem 'pg'
end
```

To install all the gems from a Gemfile do:

```bash
gem install bundler
bundle install
```

## Bundler/inline (bundler v1.10 and later)
Sometimes you need to make a script for someone but you are not sure what he has on his machine. Is there everything that your script needs? Not to worry. Bundler has a great function called in line.

It provides a `gemfile` method and before the script is run it downloads and requires all the necessary gems. A little example:

    require 'bundler/inline' #require only what you need

    #Start the bundler and in it use the syntax you are already familiar with
    gemfile(true) do 
      source 'https://rubygems.org'
            gem 'nokogiri', '~> 1.6.8.1'
            gem 'ruby-graphviz'
    end



