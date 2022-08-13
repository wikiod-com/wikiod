---
title: "Templating"
slug: "templating"
draft: false
images: []
weight: 9845
type: docs
toc: true
---

## Prevent sensitive methods from being called in templates
When an object is exposed to the template context, its arguments-less methods are available. This is useful when these functions are "getters". But it can be hazardeous if these methods alter some data or have some side effects. Eventhough you likely trust the template writer, he may not be aware of a function's side effects or think call the wrong attribute by mistake.

Given the following model:

    class Foobar(models.Model):
        points_credit = models.IntegerField()

        def credit_points(self, nb_points=1):
            """Credit points and return the new points credit value."""
            self.points_credit = F('points_credit') + nb_points
            self.save(update_fields=['points_credit'])
            return self.points_credit

If you write this, by mistake, in a template:

     You have {{ foobar.credit_points }} points!

This will increment the number of points each time the template is called. And you may not even notice it.

To prevent this, you must set the `alters_data` attribute to `True` to methods that have side effects. This will make it impossible to call them from a template.


    def credit_points(self, nb_points=1):
        """Credit points and return the new points credit value."""
        self.points_credit = F('points_credit') + nb_points
        self.save(update_fields=['points_credit'])
        return self.points_credit
    credit_points.alters_data = True

## Templating in Function Based Views
You can use a template in a function based view as follows:

    from django.shortcuts import render

    def view(request):
        return render(request, "template.html")

If you want to use template variables, you can do so as follows:

    from django.shortcuts import render

    def view(request):
        context = {"var1": True, "var2": "foo"}
        return render(request, "template.html", context=context)

Then, in `template.html`, you can refer to your variables like so:

    <html>
    {% if var1 %}
        <h1>{{ var2 }}</h1>
    {% endif %}
    </html>

## Template filters
The Django template system has built-in *tags* and *filters*, which are functions inside template to render content in a specific way. Multiple filters can be specified with pipes and filters can have arguments, just as in variable syntax.

    {{ "MAINROAD 3222"|lower }}    # mainroad 3222
    {{ 10|add:15}}                 # 25
    {{ "super"|add:"glue" }}       # superglue
    {{ "A7"|add:"00" }}            # A700
    {{ myDate | date:"D d M Y"}}   # Wed 20 Jul 2016   

 
A list of available **built-in filters** can be found at https://docs.djangoproject.com/en/dev/ref/templates/builtins/#ref-templates-builtins-filters . 



**Creating custom filters**

To add your own template filters, create a folder named `templatetags` inside your app folder. Then add a `__init__.py`, and the file your file that will contain the filters:

    #/myapp/templatetags/filters.py
    from django import template
    
    register = template.Library()
    
    @register.filter(name='tostring')
    def to_string(value):
        return str(value)

To actually use the filter you need to load it in your template:

    #templates/mytemplate.html
    {% load filters %}
    {% if customer_id|tostring = customer %} Welcome back {% endif%}



**Tricks**
 
Even though the filters seem simple at first, it allows to do some nifty things:

    {% for x in ""|ljust:"20" %}Hello World!{% endfor %}    # Hello World!Hello World!Hel...    
    {{ user.name.split|join:"_" }} ## replaces whitespace with '_' 

See also [template tags][1] for more information.

 [1]: https://www.wikiod.com/django/template-tags-and-filters


## Templating in Class Based Views
You can pass data to a template in a custom variable.

In your `views.py`:

    from django.views.generic import TemplateView
    from MyProject.myapp.models import Item

    class ItemView(TemplateView):
        template_name = "item.html"

        def items(self):
            """ Get all Items """
            return Item.objects.all()

        def certain_items(self):
            """ Get certain Items """
            return Item.objects.filter(model_field="certain")
        
        def categories(self):
            """ Get categories related to this Item """
            return Item.objects.get(slug=self.kwargs['slug']).categories.all()

A simple list in your `item.html`:

    {% for item in view.items %}
    <ul>        
        <li>{{ item }}</li>
    </ul>
    {% endfor %}


You can also retrieve additional properties of the data.

Assuming your model `Item` has a `name` field:
    
    {% for item in view.certain_items %}
    <ul>        
        <li>{{ item.name }}</li>
    </ul>
    {% endfor %}




## Variables
Variables you have provided in your view context can be accessed using double-brace notation:

In your `views.py`:

    class UserView(TemplateView):
      """ Supply the request user object to the template """
    
      template_name = "user.html"
    
      def get_context_data(self, **kwargs):
        context = super(UserView, self).get_context_data(**kwargs)
        context.update(user=self.request.user)
        return context

In `user.html`:

    <h1>{{ user.username }}</h1>
    
    <div class="email">{{ user.email }}</div>

The dot notation will access:

 - properties of the object, e.g. `user.username` will be `{{ user.username }}`
 - dictionary lookups, e.g. `request.GET["search"]` will be `{{ request.GET.search }}`
 - methods with no arguments, e.g. `users.count()` will be `{{ user.count }}`

Template variables cannot access methods that take arguments.

Variables can also be tested and looped over:

    {% if user.is_authenticated %}
      {% for item in menu %}
        <li><a href="{{ item.url }}">{{ item.name }}</a></li>
      {% endfor %}
    {% else %}
      <li><a href="{% url 'login' %}">Login</a>
    {% endif %}

URLs are accessed using `{% url 'name' %}` format, where the names correspond to names in your `urls.py`.

`{% url 'login' %}` - Will probably render as `/accounts/login/`  
`{% url 'user_profile' user.id %}` - Arguments for URLs are supplied in order  
`{% url next %}` - URLs can be variables  




## Use of {% extends %} , {% include %} and {% blocks %}
# summary
 - **{% extends %}**: this declares the template given as an argument as the current template's parent. Usage: `{% extends 'parent_template.html' %}`.

 - **{% block %}{% endblock %}**: This is used to define sections in your templates, so that if another template extends this one, it'll be able to replace whatever html code has been written inside of it. Blocks are identified by their name. Usage: `{% block content %} <html_code> {% endblock %}`.

 - **{% include %}**: this will insert a template within the current one. Be aware that the included template will receive the request's context, and you can give it  custom variables too. Basic usage: `{% include 'template_name.html' %}`, usage with variables: `{% include 'template_name.html' with variable='value' variable2=8 %}`

# Guide

Suppose you are building up your front end side code with having common layouts for all code and you do not want to repeat the code for every template. Django gives you in built tags for doing so.  
Suppose we have one blog website having 3 templates which share the same layout:

    project_directory
        ..
        templates
          front-page.html
          blogs.html
          blog-detail.html

1 ) Define `base.html` file,

    <html>
      <head>
      </head>
    
      <body>
            {% block content %}
            {% endblock %}
       </body>
    </html>

2 ) Extend it in `blog.html` like,

    {% extends 'base.html' %}
    
    {% block content %}
        # write your blog related code here
    {% endblock %}
    
    # None of the code written here will be added to the template

Here we extended the base layout so its HTML layout is now available in the `blog.html` file.The concept of `{ % block  %}` is template inheritance which allows you to build a base “skeleton” template that contains all the common elements of your site and defines blocks that child templates can override.

3 ) Now suppose all of your 3 templates also having same HTML div which defines some popular posts.Instead of being written the 3 times create one new template `posts.html`.

*blog.html*


    {% extends 'base.html' %}
    
    {% block content %}
        # write your blog related code here
        {% include 'posts.html' %} # includes posts.html in blog.html file without passing any data
        <!-- or -->
        {% include 'posts.html' with posts=postdata %} # includes posts.html in blog.html file with passing posts data which is context of view function returns.
    {% endblock %}



