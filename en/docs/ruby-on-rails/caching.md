---
title: "Caching"
slug: "caching"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Russian Doll Caching
You may want to nest cached fragments inside other cached fragments. This is called `Russian doll caching`.

The advantage of `Russian doll caching` is that if a single product is updated, all the other inner fragments can be reused when regenerating the outer fragment.

As explained in the previous section, a cached file will expire if the value of `updated_at` changes for a record on which the cached file directly depends. However, this will not expire any cache the fragment is nested within.

For example, take the following view:

    <% cache product do %>
      <%= render product.games %>
    <% end %>

Which in turn renders this view:

    <% cache game do %>
      <%= render game %>
    <% end %>

If any attribute of game is changed, the `updated_at` value will be set to the current time, thereby expiring the cache. 

However, because `updated_at` will not be changed for the product object, that cache will not be expired and your app will serve stale data. To fix this, we tie the models together with the touch method:

    class Product < ApplicationRecord
      has_many :games
    end
 
    class Game < ApplicationRecord
      belongs_to :product, touch: true
    end

## SQL Caching


## Fragment caching
`Rails.cache`, provided by ActiveSupport, can be used to cache any serializable Ruby object across requests.

To fetch a value from the cache for a given key, use `cache.read`:

    Rails.cache.read('city')
    # => nil

Use `cache.write` to write a value to the cache:

    Rails.cache.write('city', 'Duckburgh')
    Rails.cache.read('city')
    # => 'Duckburgh'

Alternatively, use `cache.fetch` to read a value from the cache and optionally write a default if there is no value:

    Rails.cache.fetch('user') do
      User.where(:is_awesome => true)
    end

The return value of the passed block will be assigned to the cache under the given key, and then returned.

You can also specify a cache expiry:

    Rails.cache.fetch('user', :expires_in => 30.minutes) do
      User.where(:is_awesome => true)
    end

## Page caching
You can use the [ActionPack page_caching gem](https://github.com/rails/actionpack-page_caching) to cache individual pages. This stores the result of one dynamic request as a static HTML file, which is served in place of the dynamic request on subsequent requests. The README contains full setup instructions. Once set up, use the `caches_page` class method in a controller to cache the result of an action:

    class UsersController < ActionController::Base
      caches_page :index
    end

Use `expire_page` to force expiration of the cache by deleting the stored HTML file:

    class UsersController < ActionController::Base
      caches_page :index
    
      def index
        @users = User.all
      end
    
      def create
        expire_page :action => :index
      end
    end

The syntax of `expire_page` mimics that of `url_for` and friends.

## HTTP caching
Rails >= 3 comes with HTTP caching abilities out of the box. This uses the `Cache-Control` and `ETag` headers to control how long a client or intermediary (such as a CDN) can cache a page.

In a controller action, use `expires_in` to set the length of caching for that action:

    def show
      @user = User.find params[:id]
      expires_in 30.minutes, :public => true
    end

Use `expires_now` to force immediate expiration of a cached resource on any visiting client or intermediary:

    def show
      @users = User.find params[:id]
      expires_now if params[:id] == 1
    end

## Action caching
Like page caching, action caching caches the whole page. The difference is that the request hits the Rails stack so before filters are run before the cache is served. 
It's extracted from Rails to [actionpack-action_caching gem][1].

A common example is caching of an action that requires authentication:

    class SecretIngredientsController < ApplicationController
      before_action :authenticate_user!, only: :index, :show
      caches_action :index
      
      def index
        @secret_ingredients = Recipe.find(params[:recipe_id]).secret_ingredients
      end
    end

Options include `:expires_in`, a custom `:cache_path` (for actions with multiple routes that should be cached differently) and `:if`/`:unless` to control when the action should be cached.

    class RecipesController < ApplicationController
      before_action :authenticate_user!, except: :show
      caches_page :show
      caches_action :archive, expires_in: 1.day
      caches_action :index, unless: { request.format.json? }
    end

When the layout has dynamic content, cache only the action content by passing `layout: false`.


  [1]: https://github.com/rails/actionpack-action_caching

