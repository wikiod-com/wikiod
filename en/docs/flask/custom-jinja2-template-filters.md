---
title: "Custom Jinja2 Template Filters"
slug: "custom-jinja2-template-filters"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
 - {{ my_date_time|my_custom_filter }}

 - {{ my_date_time|my_custom_filter(args) }}

## Parameters
| Parameter | Details | 
| --------- | ------- |
| value | The value passed in by Jinja, to be filtered |
| args | Extra arguments to be passed into the filter function |

## Format datetime in a Jinja2 template
Filters can either be defined in a method and then added to Jinja's filters dictionary, or defined in a method decorated with `Flask.template_filter`.

Defining and registering later:

    def format_datetime(value, format="%d %b %Y %I:%M %p"):
        """Format a date time to (Default): d Mon YYYY HH:MM P"""
        if value is None:
            return ""
        return value.strftime(format)

    # Register the template filter with the Jinja Environment
    app.jinja_env.filters['formatdatetime'] = format_datetime

Defining with decorator:

    @app.template_filter('formatdatetime')
    def format_datetime(value, format="%d %b %Y %I:%M %p"):
        """Format a date time to (Default): d Mon YYYY HH:MM P"""
        if value is None:
            return ""
        return value.strftime(format)


