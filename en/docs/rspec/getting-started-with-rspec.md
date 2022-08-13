---
title: "Getting started with rspec"
slug: "getting-started-with-rspec"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A simple RSpec example
In greeter.rb (wherever that goes in your project):

    class Greeter
      def greet
        "Hello, world!"
      end
    end

In spec/greeter_spec.rb:
    
    require_relative '../greeter.rb'

    RSpec.describe Greeter do
      describe '#greet' do
        it "says hello" do
          expect(Greeter.new.greet).to eq("Hello, world!")
        end
      end
    end

So our file structure looks like:

    $ tree .
    .
    ├── greeter.rb
    └── spec
        └── greeter_spec.rb
    
    1 directory, 2 files

**Output**

    $rspec greeter_spec.rb
    Finished in 0.00063 seconds (files took 0.06514 seconds to load)
    1 example, 0 failures


In RSpec terminology, the file is a "spec" of `Greeter` and the `it` block is an "example". The line with `expect` is an expectation. If the expectation is met, nothing happens and the test passes. If not, the test fails.

This example also shows that `describe` blocks can be nested, in this case to convey that the `greet` method is part of the `Greet` class. The `#` in `#greet` is only a convention to show that `greet` is an instance method (as opposed to '.' for a class method). RSpec doesn't interpret the string at all, so you could use a different string or omit that `describe` block entirely.

## Installing RSpec
The most common way to install the RSpec gem is using [Bundler](http://bundler.io/). Add this line to your application's `Gemfile`:

    gem 'rspec'

And then execute `bundle` to install the dependencies:

    $ bundle

Alternatively, you can install the gem manually:

    $ gem install rspec

After installing the gem, run the following command:

    rspec --init

This will create a `spec` folder for your tests, along with the following config files:

- a `spec` directory into which to put spec files
- a `spec/spec_helper.rb` file with default configuration options
- an `.rspec` file with default command-line flags


