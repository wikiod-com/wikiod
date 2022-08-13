---
title: "Context Processors"
slug: "context-processors"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

Use context processors to add variables that are accessible anywhere in your templates.

Specify a function, or functions that return `dict`s of the variables you want, then add those functions to `TEMPLATE_CONTEXT_PROCESSORS`.

## Use a context processor to access settings.DEBUG in templates
in `myapp/context_processors.py`:

    from django.conf import settings
    
    def debug(request):
      return {'DEBUG': settings.DEBUG}

in `settings.py`:

    TEMPLATES = [
        {
            ...
            'OPTIONS': {
                'context_processors': [
                    ...
                    'myapp.context_processors.debug',
                ],
            },
        },
    ]

or, for versions < 1.9:

    TEMPLATE_CONTEXT_PROCESSORS = (
        ...
        'myapp.context_processors.debug',
    )

Then in my templates, simply:

     {% if DEBUG %} .header { background:#f00; } {% endif %}
     {{ DEBUG }}

## Using a context processor to access your most recent blog entries in all templates
Assuming you have a model called `Post` defined in your `models.py` file that contains blog posts, and has a `date_published` field. 


----------


**Step 1: Write the context processor**

Create (or add to) a file in your app directory called `context_processors.py`:

    from myapp.models import Post
    
    def recent_blog_posts(request):
        return {'recent_posts':Post.objects.order_by('-date_published')[0:3],}  # Can change numbers for more/fewer posts


----------


**Step 2: Add the context processor to your settings file**

Make sure that you add your new context processor to your `settings.py` file in the `TEMPLATES` variable:

    TEMPLATES = [
        {
            ...
            'OPTIONS': {
                'context_processors': [
                    ...
                    'myapp.context_processors.recent_blog_posts',
                ],
            },
        },
    ]

(In Django versions before 1.9, this was set directly in `settings.py` using a `TEMPLATE_CONTEXT_PROCESSORS` [variable][1].)


----------
**Step 3: Use the context processor in your templates**

No need to pass recent blog entries through individual views anymore!  Just use `recent_blog_posts` in any template.

E.g., in `home.html` you could create a sidebar with links to recent posts:

    <div class="blog_post_sidebar">
        {% for post in recent_blog_posts %}
            <div class="post">
                <a href="{{post.get_absolute_url}}">{{post.title}}</a>
            </div>
        {% endfor %}
    </div>

Or in `blog.html` you could create a more detailed display of each post:

    <div class="content">
        {% for post in recent_blog_posts %}
            <div class="post_detail">
                <h2>{{post.title}}</h2>
                <p>Published on {{post.date_published}}</p>
                <p class="author">Written by: {{post.author}}</p>
                <p><a href="{{post.get_absolute_url}}">Permalink</a></p>
                <p class="post_body">{{post.body}}</p>
            </div>
        {% endfor %}
    </div>



  [1]: https://docs.djangoproject.com/en/1.8/ref/settings/#template-context-processors

## Extending your templates
Context processor to determine the template based on group membership(or any query/logic).  This allows our public/regular users to get one template and our special group to get a different one.

myapp/context_processors.py

    def template_selection(request):
        site_template = 'template_public.html'
        if request.user.is_authenticated():
            if request.user.groups.filter(name="some_group_name").exists():
                site_template = 'template_new.html'
    
        return {
            'site_template': site_template,
        }

Add the context processor to your settings.

In your templates, use the variable defined in the context processor.

    {% extends site_template %}

