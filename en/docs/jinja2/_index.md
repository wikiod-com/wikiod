---
title : jinja2 Tutorial
slug : jinja2-tutorial
weight : 9995
draft : false
images : []
type : docs
---

Jinja 2 is a templating engine for Python, which means that it allows developer to produce web pages, containing for example base html code and placeholders for Jinja 2 to fill them. Based upon Django's templating system, Jinja is one of the most used as it allows developers to use powerful concepts like sandboxing and inheritance to allow a template easily reused.

Jinja is simple. You have a template with a bunch of holes in it. You then ask the engine to fill the template with the values you give it at runtime, and ther response is handed back to you, in form of an html document, ready to be sent to the user.
You also have more advanced possibilities like applying a filter on a variable, to show for example a read time based on an article page for a blog, or simply pluralize words like a breeze.

You can read more on Jinja2 through the official documentation [here][1]

  [1]: http://jinja.pocoo.org/

