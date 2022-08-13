---
title: "Decorator pattern"
slug: "decorator-pattern"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

The **[Decorator pattern](https://en.wikipedia.org/wiki/Decorator_pattern)** allows you to add or modify behavior of objects in a situational way without affecting the base object.

This can be achieved though plain Ruby using the stdlib, or via popular gems such as [Draper](https://github.com/drapergem/draper).

## Decorating a Model using SimpleDelegator
Most Rails developers start by modifying their model information within the template itself:

    <h1><%= "#{ @user.first_name } #{ @user.last_name }" %></h1>
    <h3>joined: <%= @user.created_at.in_time_zone(current_user.timezone).strftime("%A, %d %b %Y %l:%M %p") %></h3>

For models with a lot of data, this can quickly become cumbersome and lead to copy-pasting logic from one template to another.

This example uses `SimpleDelegator` from the stdlib.

All requests to a `SimpleDelegator` object are passed to the parent object by default. You can override any method with presentation logic, or you can add new methods that are specific to this view.

`SimpleDelegator` provides two methods: `__setobj__` to set what object is being delegated to, and `__getobj__` to get that object.

    class UserDecorator < SimpleDelegator
      attr_reader :view
      def initialize(user, view)
        __setobj__ @user
        @view = view
      end
    
      # new methods can call methods on the parent implicitly
      def full_name
        "#{ first_name } #{ last_name }"
      end
    
      # however, if you're overriding an existing method you need
      # to use __getobj__
      def created_at
        Time.use_zone(view.current_user.timezone) do
          __getobj__.created_at.strftime("%A, %d %b %Y %l:%M %p")
        end
      end
    end

Some decorators rely on magic to wire-up this behavior, but you can make it more obvious where the presentation logic is coming from by initializing the object on the page.

    <% user = UserDecorator.new(@user, self) %>
    <h1><%= user.full_name %></h1>
    <h3>joined: <%= user.created_at %></h3>

By passing a reference to the view object into the decorator, we can still access all of the rest of the view helpers while building the presentation logic without having to include it.

Now the view template is only concerned with inserting data into the page, and it is much more clear.




## Decorating a Model using Draper
Draper automatically matches up models with their decorators by convention.

    # app/decorators/user_decorator.rb
    class UserDecorator < Draper::Decorator
      def full_name
        "#{object.first_name} #{object.last_name}"
      end

      def created_at
        Time.use_zone(h.current_user.timezone) do
          object.created_at.strftime("%A, %d %b %Y %l:%M %p")
        end
      end
    end

Given a `@user` variable containing an ActiveRecord object, you can access your decorator by calling `#decorate` on the `@user`, or by specifying the Draper class if you want to be specific.

    <% user = @user.decorate %><!-- OR -->
    <% user = UserDecorator.decorate(@user) %>
    <h1><%= user.full_name %></h1>
    <h3>joined: <%= user.created_at %></h3>



