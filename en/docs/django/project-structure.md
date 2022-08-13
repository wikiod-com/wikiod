---
title: "Project Structure"
slug: "project-structure"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Repository > Project > Site/Conf
For a Django project with `requirements` and `deployment tools` under source control. This example builds upon concepts from the [Two Scoops of Django][1]. They have published a [template][2]:

    repository/
        docs/
        .gitignore
        project/
            apps/
                blog/
                    migrations/
                    static/ #( optional )
                        blog/
                            some.css
                    templates/ #( optional )
                        blog/
                            some.html
                    models.py
                    tests.py
                    admin.py
                    apps.py #( django 1.9 and later )
                    views.py
                accounts/
                    #... ( same as blog )
                search/
                    #... ( same as blog )
            conf/
                settings/
                    local.py
                    development.py
                    production.py
                wsgi
                urls.py
            static/
            templates/
        deploy/
            fabfile.py
        requirements/
            base.txt
            local.txt
        README
        AUTHORS
        LICENSE
Here `apps` and `conf` folders contain `user created applications` and `core configuration folder` for the project respectively.

`static` and `templates` folders in `project` directory contains static files and `html markup` files respectively that are being used globally throughout the project.

And all app folders `blog`, `accounts` and `search` may also ( mostly ) contain `static` and `templates` folders.


  [1]: https://www.twoscoopspress.com/
  [2]: https://github.com/twoscoops/django-twoscoops-project

## Namespacing static and templates files in django apps
`static` and `templates` folder in the apps may should also contain a folder with the name of app `ex. blog` this is a convention used to prevent namespace pollution, so we reference the files like `/blog/base.html` rather than `/base.html` which provides more clarity about the file we are referencing and preserves namespace.

Example: `templates` folder inside `blog` and `search` applications contains a file with name `base.html`, and when referencing the file in `views` your application gets confused in which file to render.

    (Project Structure)
    .../project/
        apps/
            blog/
                templates/
                    base.html
            search/
                templates/
                    base.html

    (blog/views.py)
    def some_func(request):
        return render(request, "/base.html")

    (search/views.py)
    def some_func(request):
        return render(request, "/base.html")

    ## After creating a folder inside /blog/templates/(blog) ##
    
    (Project Structure)
    .../project/
        apps/
            blog/
                templates/
                    blog/
                        base.html
            search/
                templates/
                    search/
                        base.html

    (blog/views.py)
    def some_func(request):
        return render(request, "/blog/base.html")

    (search/views.py)
    def some_func(request):
        return render(request, "/search/base.html")

