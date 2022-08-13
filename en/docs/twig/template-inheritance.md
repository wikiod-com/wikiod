---
title: "Template inheritance"
slug: "template-inheritance"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Using a base template
base.twig.html
--------------

    <!DOCTYPE html>
    <html>
        <head>
            <title>{{ title | default('Hello World') }}</title>
            <link rel="stylesheet" type="text/css" href="theme.css">
            {% block css %}
            {% endblock %}
        </head>
        <body>
        {% block body %}
            <nav>
            {% block navigation %}
                <a href="#">Link</a>
                <a href="#">Link</a>
                <a href="#">Link</a>
            {% endblock navigation %}
            </nav>
            <section id="container">
                <section id="content">
                {% block content %}
                <p>
                Lorem ipsum dolor sit amet.
                </p>
            </section>
            {% endblock content %}
        </section>
        {% endblock body %}
        </body>
    </html>

page.twig.html

    {% extends base.twig.html %}
    {% block navigation %}
    <a href="page2.html">Page 2</a>
    <a href="page3.html">Page 3</a>
    <a href="page4.html">Page 4</a>
    {% endblock %}
    {% block content %}
    This is my first page
    {% endblock content %}

--------------

        

## Change the content of an included template
article.twig.html
-----------------

    <article>
        <h1>{{ article.title }}</h1>
        {% block content %}
        <p>{{ article.content }}</p>
        {% endblock %}
    </article>

articles.twig.html
------------------

    {# use default template for article #}
    {% for article in articles %}
        {% include "article.twig.html" %}
    {% endfor %}

-----   
    {# use custom template for article #}
    {% for article in articles %}
        {% embed "article.twig.html" %}
            {% block content %}
                <img src="{{ article.image }}" alt="{{ article.title }}" />
                {{ parent() }}
            {% endblock %}
        {% endembed %}
    {% endfor %}



## Variable inheritance


parent.html.twig
----------------



    <!DOCTYPE html>
    <html>
        <head>
            <title>{{ title_variable | default('Normal Page') }}</title>
        </head>
        <body>
             <h1>This is the {{ title_variable }} Page</h1>    
        </body>
    </html>
         


----------


child.html.twig
=======



    {% extends "parent.html.twig" %}
    {% set title_variable = "Child" %}

## Comparison of Include, Extends, Use, Macro, Embed
There are various types of inheritance and code reuse in Twig:

# Include
The main goal is **code reuse**. Consider using `header.html.twig` & `footer.html.twig` inside `base.html.twig` as an example.

**header.html.twig**

    <nav>
       <div>Homepage</div>
       <div>About</div>
    </nav>
---
**base.html.twig**

    {% include 'header.html.twig' %}
    <main>{% block main %}{% endblock %}</main>

# Extends
The main goal is **vertical inheritance**. Consider extending `base.html.twig` inside `homepage.html.twig` and `about.html.twig` as an example.

**base.html.twig**

    {% include 'header.html.twig' %}
    <main>{% block main %}{% endblock %}</main>

---
**homepage.html.twig**

    {% extends 'base.html.twig' %}

    {% block main %}
    <p>You are at the homepage</p>
    {% endblock %}
---
**about.html.twig**

    {% extends 'base.html.twig' %}

    {% block main %}
    <p>You are at the about page</p>
    {% endblock %}

# Use
The main goal is **horizontal reuse**. Consider using `sidebar.product.html.twig` inside `single.product.html.twig` (extends `product.layout.html.twig`) and `single.service.html.twig` (extends 'service.layout.html.page') pages. (it's like macros, but for blocks)

**sidebar.html.twig**

    <aside>{% block sidebar %}{% endblock %}</aside>

---
**single.product.html.twig**

    {% extends 'product.layout.html.twig' %}

    {% use 'sidebar.html.twig' %}
    {% block main %}
    <p>You are at the product page for product number 123</p>
    {% endblock %}
---
**single.service.html.twig**

    {% extends 'service.layout.html.twig' %}

    {% use 'sidebar.html.twig' %}
    {% block main %}
    <p>You are at the service page for service number 456</p>
    {% endblock %}

# Macro
The main goal is **having reusable markup across many templates with variables**. Consider a function which gets some variables and outputs some markup.

**form.html.twig**

{% macro input(name, value, type) %}
    <input type="{{ type|default('text') }}" name="{{ name }}" value="{{ value|e }}" }}" />
{% endmacro %}
---
**profile.service.html.twig**

    {% import "forms.html.twig" as forms %}

    <div>{{ forms.input('username') }}</div>

# Embed
The main goal is **block overriding**. It has functionality of both `Use` & `Include` together. Consider embedding `pagination.html.twig` in `product.table.html.twig` & `service.table.html.twig`.

**pagination.html.twig**

    <div>
        <div>{% block first %}{% endblock %}</div>
        {% for i in (min + 1)..(max - 1) %}
            <div>{{ i }}</div>
        {% endfor %}
        <div>{% block last %}{% endblock %}</div>
    </div>

---
**product.table.html.twig**

    {% set min, max = 1, products.itemPerPage %}
    
    {% embed 'pagination.html.twig' %}
        {% block first %}First Product Page{% endblock %}
        {% block last %}Last Product Page{% endblock %}
    {% endembed %}
---
**service.table.html.twig**

    {% set min, max = 1, services.itemPerPage %}
    
    {% embed 'pagination.html.twig' %}
        {% block first %}First Service Page{% endblock %}
        {% block last %}Last Service Page{% endblock %}
    {% endembed %}
Please note that embedded file (`pagination.html.twig` here) has access to the current context (`min`, `max` variables here). Also you may pass extra variables to the embedded file:

**pagination.html.twig**

    <p>{{ count }} items</p>
    <div>
        <div>{% block first %}{% endblock %}</div>
        {% for i in (min + 1)..(max - 1) %}
            <div>{{ i }}</div>
        {% endfor %}
        <div>{% block last %}{% endblock %}</div>
    </div>

---
**product.table.html.twig**

    {% set min, max = 1, products|length %}
    
    {% embed 'pagination.html.twig' with {'count': products|length } %}
        {% block first %}First Product Page{% endblock %}
        {% block last %}Last Product Page{% endblock %}
    {% endembed %}

