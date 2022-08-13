---
title: "Generic Views"
slug: "generic-views"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Generic views are views that perform a certain pre-defined action, like creating, editing or deleting objects, or simply showing a template. 

Generic views have to be distinguished from functional views, which are always hand-written to perform the required tasks. In a nutshell, it can be said that generic views need to be configured, while functional views need to be programmed.

Generic views may save a lot of time, especially when you have many standardized tasks to perform.



These examples show that generic views generally make standardized tasks much simpler. Instead of programming everything from scratch, you configure what other people have already programmed for you. This makes sense in many situations, as it allows you concentrate more on the design of your projects rather than the processes in the background.

So, should you *always* use them? No. They only make sense as long as your tasks are fairly standardizes (loading, editig, deleting objects) and the more repetitive your tasks are. Using one specific generic view only once and then override all its methods to perform very speficic tasks may not make sense. You may be better off with a functional view here.

However, if you have plenty of views that require this functionality or if your tasks match excatly the defined tasks of a specific generic view, then generic views are exactly what you need in order to make your life simpler.

## Minimum Example: Functional vs. Generic Views
Example for a functional view to create an object. Excluding comments and blank lines, we need 15 lines of code:

    # imports
    from django.shortcuts import render_to_response
    from django.http import HttpResponseRedirect

    from .models import SampleObject
    from .forms import SampleObjectForm
    
    # view functioon
    def create_object(request):
        
        # when request method is 'GET', show the template
        if request.method == GET:
            # perform actions, such as loading a model form
            form = SampleObjectForm()
            return render_to_response('template.html', locals())
        
        # if request method is 'POST', create the object and redirect
        if request.method == POST:
            form = SampleObjectForm(request.POST)

            # save object and redirect to success page if form is valid
            if form.is_valid:
                form.save()
                return HttpResponseRedirect('url_to_redirect_to')

            # load template with form and show errors
            else:
                return render_to_response('template.html', locals())

Example for a 'Class-Based Generic View' to perform the same task. We only need 7 lines of code to achieve the same task:
    
    from django.views.generic import CreateView
    
    from .models import SampleObject
    from .forms import SampleObjectForm

    class CreateObject(CreateView):
        model = SampleObject
        form_class = SampleObjectForm
        success_url = 'url_to_redirect_to'
        
    

## Customizing Generic Views
The above example only works if your tasks are entirely standard tasks. You do not add extra context here, for example. 

Let's make a more realistic example. Assume we want to add a page title to the template. In the functional view, this would work like this - with just one additional line:

    def create_object(request):
        page_title = 'My Page Title'

        # ...

        return render_to_response('template.html', locals())

  
This is more difficult (or: counter-intutitive) to achieve with generic views. As they are class-based, you need to override one or several of the class's method to achieve the desired outcome. In our example, we need to override the class's *get_context_data* method like so:

    class CreateObject(CreateView):
        model = SampleObject
        form_class = SampleObjectForm
        success_url = 'url_to_redirect_to'

        def get_context_data(self, **kwargs):
            
            # Call class's get_context_data method to retrieve context
            context = super().get_context_data(**kwargs) 
            
            context['page_title'] = 'My page title'
            return context

Here, we need four additional lines to code instead of just one - at least for the *first* additional context variable we want to add.


## Generic Views with Mixins
The true power of generic views unfolds when you combine them with Mixins. A mixin is a just another class defined by you whose methods can be inherited by your view class. 

Assume you want every view to show the additional variable 'page_title' in the template. Instead of overriding the get_context_data method each time you define the view, you create a mixin with this method and let your views inherit from this mixin. Sounds more complicated than it actually is:

    # Your Mixin
    class CustomMixin(object):
        
        def get_context_data(self, **kwargs):
            
            # Call class's get_context_data method to retrieve context
            context = super().get_context_data(**kwargs) 
            
            context['page_title'] = 'My page title'
            return context

    # Your view function now inherits from the Mixin
    class CreateObject(CustomMixin, CreateView):
        model = SampleObject
        form_class = SampleObjectForm
        success_url = 'url_to_redirect_to'

    # As all other view functions which need these methods
    class EditObject(CustomMixin, EditView):
        model = SampleObject
        # ...

The beauty of this is that your code becomes much more structured than it is mostly the case with functional views. Your entire logic behind specific tasks sits in one place and one place only. Also, you will save tremendous amounts of time especially when you have many views that always perform the same tasks, except with different objects

