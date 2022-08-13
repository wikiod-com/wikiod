---
title: "How to use Django with Cookiecutter?"
slug: "how-to-use-django-with-cookiecutter"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Installing and setting up django project using Cookiecutter
Following are the Prerequisites for installing Cookiecutter:

 - pip
 - virtualenv
 - PostgreSQL

Create a virtualenv for your project and activate it:

    $ mkvirtualenv <virtualenv name>
    $ workon <virtualenv name>
Now install Cookiecutter using:

    $ pip install cookiecutter
Change directories into the folder where you want your project to live. Now execute the following command to generate a django project:

    $ cookiecutter https://github.com/pydanny/cookiecutter-django.git

This command runs cookiecutter with the cookiecutter-django repo, asking us to enter project-specific details. Press “enter” without typing anything to use the default values, which are shown in [brackets] after the question.

    project_name [project_name]: example_project
    repo_name [example_project]: 
    author_name [Your Name]: Atul Mishra
    email [Your email]: abc@gmail.com
    description [A short description of the project.]: Demo Project
    domain_name [example.com]: example.com
    version [0.1.0]: 0.1.0
    timezone [UTC]: UTC
    now [2016/03/08]: 2016/03/08
    year [2016]: 2016
    use_whitenoise [y]: y
    use_celery [n]: n
    use_mailhog [n]: n
    use_sentry [n]: n
    use_newrelic [n]: n
    use_opbeat [n]: n
    windows [n]: n
    use_python2 [n]: n


More details about the project generation options can be found in the [offical documentation.][1]
The project is now setup. 


  [1]: https://cookiecutter-django.readthedocs.io/en/latest/project-generation-options.html

