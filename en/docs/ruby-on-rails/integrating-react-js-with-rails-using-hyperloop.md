---
title: "Integrating React.js with Rails Using Hyperloop"
slug: "integrating-reactjs-with-rails-using-hyperloop"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

This topic covers integrating React.js with Rails using the [Hyperloop](http://ruby-hyperloop.io) gem

Other approaches not covered here are using the react-rails or react_on_rails gems.

Component classes simply generate the equivalent javascript component classes.

You can also access javascript components and libraries directly from your ruby component classes.

Hyperloop will "prerender" the view server side so your initial view will load just like ERB or HAML templates.  Once loaded on the client react takes over and will incrementally update the DOM as state changes due to inputs from the user, HTTP requests or incoming web socket data.

Besides Components, Hyperloop has Stores to manage shared state, Operations to encapsulate isomorphic business logic, and Models which give direct access to your ActiveRecord models on the client using the standard AR syntax.

More info here: http://ruby-hyperloop.io/

## Adding a simple react component (written in ruby) to your Rails app
1. Add the hyperloop gem to your rails (4.0 - 5.1) Gemfile
2. `bundle install`
3. Add the hyperloop manifest to the application.js file:  
    ```javascript
    // app/assets/javascripts/application.js
    ...
    //= hyperloop-loader
    ```
4. Create your react components, and place them in the `hyperloop/components` directory
    ```ruby
    # app/hyperloop/components/hello_world.rb
    class HelloWorld < Hyperloop::Component
      after_mount do
        every(1.second) { mutate.current_time(Time.now) }
      end
      render do
        "Hello World!  The time is now: #{state.current_time}"
      end
    end
    ```
5. Components act just like views.  They are "mounted" using the `render_component` method in a controller:  
    ```ruby
    # somewhere in a controller:
      ...
      def hello_world
        render_component # renders HelloWorld based on method name
      end
    ```


## Callbacks
```ruby
# all react callbacks are supported using active-record-like syntax

class SomeCallBacks < Hyperloop::Component
  before_mount do
    # initialize stuff - replaces normal class initialize method
  end
  after_mount do
    # any access to actual generated dom node, or window behaviors goes here
  end
  before_unmount do
    # any cleanups (i.e. cancel intervals etc)
  end
  
  # you can also specify a method the usual way:
  before_mount :do_some_more_initialization
end

## Declaring component parameters (props)
```ruby
class Hello < Hyperloop::Component
  # params (= react props) are declared using the param macro
  param :guest
  render do
    "Hello there #{params.guest}"
  end
end

# to "mount" Hello with guest = "Matz" say 
  Hello(guest: 'Matz')

# params can be given a default value:
  param guest: 'friend' # or
  param :guest, default: 'friend'

## HTML Tags
```ruby
# HTML tags are built in and are UPCASE
class HTMLExample < Hyperloop::Component
  render do
    DIV do
      SPAN { "Hello There" }
      SPAN { "Welcome to the Machine!" }
    end
  end
end
```

## Event Handlers
```ruby
# Event handlers are attached using the 'on' method
class ClickMe < Hyperloop::Component
  render do
    DIV do
      SPAN { "Hello There" }
      A { "Click Me" }.on(:click) { alert('you did it!' }
    end
  end
end
```

## States
```ruby
# States are read using the 'state' method, and updated using 'mutate'
# when states change they cause re-render of all dependent dom elements

class StateExample < Hyperloop::Component
  state count: 0  # by default states are initialized to nil
  render do
    DIV do
      SPAN { "Hello There" }
      A { "Click Me" }.on(:click) { mutate.count(state.count + 1) }
      DIV do 
        "You have clicked me #{state.count} #{'time'.pluralize(state.count)}"
      end unless state.count == 0
    end
  end
end
```
Note that states can be shared between components using [Hyperloop::Stores](http://ruby-hyperloop.io/start/stores/)


