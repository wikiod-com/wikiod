---
title: "ActionController"
slug: "actioncontroller"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Action Controller is the C in MVC. After the router has determined which controller to use for a request, the controller is responsible for making sense of the request and producing the output.

The controller will receive the request, fetch or save data from a model and use a view to create output. A controller can be thought of as a middleman between models and views. It makes the model data available to the view so it can display to the user, and it saves or updates user data to the model.

## Basic REST Controller


## Output JSON instead of HTML


## Controllers (Basic)


## Parameters


## Filtering parameters (Basic)


## Redirecting


## Using Views


## Display error pages for exceptions


## Filters
Filters are methods that are run "before", "after" or "around" a controller action. They are inherited, so if you set any in your `ApplicationController` they will be run for every request your application receives.

**Before Filter**

Before filters are executed before the controller action and can halt the request (and/or redirect). A common use is to verify if a user is logged in:

    class ApplicationController < ActionController::Base
      before_action :authenticate_user!

      def authenticate_user!
        redirect_to some_path unless user_signed_in?
      end
    end

Before filters are run on requests before the request gets to the controller’s action. It can return a response itself and completely bypass the action.

Other common uses of before filters is validating a user’s authentication before granting them access to the action designated to handle their request. I’ve also seen them used to load a resource from the database, check permissions on a resource, or manage redirects under other circumstances.

**After Filter**

After filters are similar to "before" ones, but as they get executed after the action run they have access the response object that's about to be sent. So in short after filters are run after the action completes. It can modify the response. Most of the time if something is done in an after filter, it can be done in the action itself, but if there is some logic to be run after running any of a set of actions, then an after filter is a good place to do it.

Generally, I’ve seen after and around filters used for logging.

**Around Filter**

Around filters may have logic before and after the action being run. It simply yields to the action in whatever place is necessary. Note that it doesn’t need to yield to the action and may run without doing so like a before filter.

Around filters are responsible for running their associated actions by yielding, similar to how Rack middlewares work.

Around callbacks wrap the execution of actions. You can write an around callback in two different styles. In the first, the callback is a single chunk of code. That code is called before the action is executed. If the callback code invokes yield, the action is executed. When the action completes, the callback code continues executing. Thus, the code before the yield is like a before action callback and the code after the yield is the after action callback. If the callback code never invokes yield. the action is not run-this is the same as having a before action callback return false.

Here's an example of the around filter:

    around_filter :catch_exceptions
     
    private
      def catch_exceptions
        begin
          yield
        rescue Exception => e 
          logger.debug "Caught exception! #{e.message}"
        end
      end

This will catch exception of any action and put the message in your log. You can use around filters for exception handling, setup and teardown, and a myriad of other cases.

**Only and Except**

All filters can be applied to specific actions, using `:only` and `:except`:

    class ProductsController < ApplicationController
      before_action :set_product, only: [:show, :edit, :update]

      # ... controller actions

      # Define your filters as controller private methods
      private

      def set_product
        @product = Product.find(params[:id])
      end
    end

**Skipping Filter**

All filters (inherited ones too) can also be skipped for some specific actions:

    class ApplicationController < ActionController::Base
      before_action :authenticate_user!

      def authenticate_user!
        redirect_to some_path unless user_signed_in?
      end
    end

    class HomeController < ApplicationController
      skip_before_action :authenticate_user!, only: [:index]

      def index
      end
    end

As they're inherited, filters can also be defined in a `namespace` "parent" controller. Say for example that you have an `admin` namespace, and you of course want only admin users to be able to access it. You could do something like this:

    # config/routes.rb
    namespace :admin do
      resources :products
    end

    # app/controllers/admin_controller.rb
    class AdminController < ApplicationController
      before_action :authenticate_admin_user!

      private

      def authenticate_admin_user!
        redirect_to root_path unless current_user.admin?
      end
    end

    # app/controllers/admin/products_controller.rb
    class Admin::ProductsController < AdminController
      # This controller will inherit :authenticate_admin_user! filter
    end

Beware that in **Rails 4.x** you could use `before_filter` along with `before_action`, but `before_filter` is currently deprecated in **Rails 5.0.0** and will be removed in **5.1**.

## Generating a controller
Rails provides a lot of generators, for controllers too of course.

You can generate a new controller by running this command in your app folder

    rails generate controller NAME [action action] [options]
_Note: You can also use `rails g` alias to invoke `rails generate`_
    
For example, to generate a controller for a `Product` model, with `#index` and `#show` actions you would run

    rails generate controller products index show

This will create the controller in `app/controllers/products_controller.rb`, with both the actions you specified

    class ProductsController < ApplicationController
      def index
      end

      def show
      end
    end

It will also create a `products` folder inside `app/views/`, containing the two templates for your controller's actions (i.e. `index.html.erb` and `show.html.erb`, _note that the extension may vary according to your template engine, so if you're using `slim`, for example, generator will create `index.html.slim` and `show.html.slim`_ )

Furthermore, if you specified any actions they will also be added to your `routes` file

    # config/routes.rb
    get 'products/show'
    get 'products/index'

Rails creates a helper file for you, in `app/helpers/products_helper.rb`, and also the assets files in `app/assets/javascripts/products.js` and `app/assets/stylesheets/products.css`. As for views, the generator changes this behaviour according to what's specified in your `Gemfile`: i.e., if you're using `Coffeescript` and `Sass` in your application, the controller generator will instead generator `products.coffee` and `products.sass`.

At last, but not least, Rails also generates test files for your controller, your helper and your views.

If you don't want any of these to be created for you can tell Rails to skip them, just prepend any option with 

`--no-` or `--skip`, like this:

    rails generate controller products index show --no-assets --no-helper
And the generator will skip both `assets` and `helper`

If you need to create a controller for a specific **`namespace`** add it in front of `NAME`:

    rails generate controller admin/products
This will create your controller inside `app/controllers/admin/products_controller.rb`

Rails can also generate a complete RESTful controller for you:

    rails generate scaffold_controller MODEL_NAME # available from Rails 4
    rails generate scaffold_controller Product




## Rescuing ActiveRecord::RecordNotFound with redirect_to
You can rescue a RecordNotFound exception with a redirect instead of showing an error page:

    class ApplicationController < ActionController::Base

      # your other stuff

      rescue_from ActiveRecord::RecordNotFound do |exception|
        redirect_to root_path, 404, alert: I18n.t("errors.record_not_found")
      end
    end

## 404 when record not found
Rescue from record not found error instead of showing an exception or white page:

    class ApplicationController < ActionController::Base
      
      # ... your other stuff here 

      rescue_from ActiveRecord::RecordNotFound do |exception|
        redirect_to root_path, 404, alert: 'Record not found'
      end
    end

