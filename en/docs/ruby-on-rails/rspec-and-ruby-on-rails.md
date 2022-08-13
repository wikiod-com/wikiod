---
title: "RSpec and Ruby on Rails"
slug: "rspec-and-ruby-on-rails"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

RSpec is a test framework for Ruby or, as defined by the official documentation, _RSpec is a Behaviour-Driven Development tool for Ruby programmers_.

This topic covers the basic of using [RSpec](http://rspec.info/) with Ruby on Rails. For specific information about RSpec, visit the [RSpec topic](https://www.wikiod.com/rspec).

## Installing RSpec
If you want to use RSpec for a Rails project, you should use the [`rspec-rails`](https://rubygems.org/gems/rspec-rails) gem, which  can generate helpers and spec files for you automatically (for example, when you create models, resources or scaffolds using `rails generate`).

Add `rspec-rails` to both the `:development` and `:test` groups in the `Gemfile`:

    group :development, :test do
      gem 'rspec-rails', '~> 3.5'
    end

Run `bundle` to install the dependencies.

Initialize it with:

    rails generate rspec:install

This will create a `spec/` folder for your tests, along with the following configuration files:

 - `.rspec` contains default options for the command-line `rspec` tool
 - `spec/spec_helper.rb` includes basic RSpec configuration options
 - `spec/rails_helper.rb` adds further configuration options that are more specific to use RSpec and Rails together.

All these files are written with sensible defaults to get you started, but you can add features and change configurations to suit your needs as your test suite grows.


