---
title: "Django from the command line."
slug: "django-from-the-command-line"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

While Django is primarily for web apps it has a powerful and easy to use ORM that can be used for command line apps and scripts too. There are two different approaches that can be used. The first being to create a custom management command and the second to initialize the Django environment at the start of your script. 

## Django from the command line.
Supposing you have setup a django project, and the settings file is in an app named main, this is how you initialize your code

    import os, sys
    
    # Setup environ
    sys.path.append(os.getcwd())
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", "main.settings")
    
    # Setup django
    import django
    django.setup()
    
    # rest of your imports go here
    
    from main.models import MyModel
    
    # normal python code that makes use of Django models go here
    
    for obj in MyModel.objects.all():
        print obj


The above can be executed as

    python main/cli.py



