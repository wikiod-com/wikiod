---
title: "Basic of Strong Parameters"
slug: "basic-of-strong-parameters"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## What is strong parameters?
As [edgeapi][1] suggests, it provides an interface for protecting attributes from end-user assignment. This makes Action Controller parameters forbidden to be used in Active Model mass assignment until they have been whitelisted. 

In addition, parameters can be marked as required and flow through a predefined raise/rescue flow to end up as a 400 Bad Request with no effort. Please review the `PeopleController` file and the comments listed to see how to use strong parameters? 

    class PeopleController < ActionController::Base
      # Using "Person.create(params[:person])" would raise an
      # ActiveModel::ForbiddenAttributesError exception because it'd
      # be using mass assignment without an explicit permit step.
      # This is the recommended form:
      def create
        Person.create(person_params)
      end
    
      # This will pass with flying colors as long as there's a person key in the
      # parameters, otherwise it'll raise an ActionController::MissingParameter
      # exception, which will get caught by ActionController::Base and turned
      # into a 400 Bad Request reply.
      def update
        redirect_to current_account.people.find(params[:id]).tap { |person|
          person.update!(person_params)
        }
      end
    
      private
        # Using a private method to encapsulate the permissible parameters is
        # just a good pattern since you'll be able to reuse the same permit
        # list between create and update. Also, you can specialize this method
        # with per-user checking of permissible attributes.
        def person_params
          params.require(:person).permit(:name, :age)
        end
    end


  [1]: http://edgeapi.rubyonrails.org/classes/ActionController/StrongParameters.html

