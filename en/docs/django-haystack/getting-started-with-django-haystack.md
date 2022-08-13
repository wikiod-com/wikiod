---
title: "Getting started with django-haystack"
slug: "getting-started-with-django-haystack"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## Installing the haystack package

    pip install django-haystack


## Configuration

Add `haystack` to your project's `INSTALLED_APPS` inside of your `settings.py` file:

<!-- language-all: lang-python -->

    # settings.py
    INSTALLED_APPS = [
        'django.contrib.admin',
        'django.contrib.auth',
        'django.contrib.contenttypes',

        # Put haystack with above your project's apps
        'haystack',
        
        'myproject_app',
    ]

Now add the settings for your search backend. Haystack currently supports: solr, elasticsearch, whoosh, and xapian.

**Solr:**

    HAYSTACK_CONNECTIONS = {
        'default': {
            'ENGINE': 'haystack.backends.solr_backend.SolrEngine',
            'URL': 'http://127.0.0.1:8983/solr'
            # ...or for multicore...
            # 'URL': 'http://127.0.0.1:8983/solr/mysite',
        },
    }

**Elasticsearch:**

    HAYSTACK_CONNECTIONS = {
        'default': {
            'ENGINE': 'haystack.backends.elasticsearch_backend.ElasticsearchSearchEngine',
            'URL': 'http://127.0.0.1:9200/',
            'INDEX_NAME': 'haystack',
        },
    }
    
**Whoosh:**

    import os
    
    HAYSTACK_CONNECTIONS = {
        'default': {
            'ENGINE': 'haystack.backends.whoosh_backend.WhooshEngine',
            'PATH': os.path.join(os.path.dirname(__file__), 'whoosh_index'),
        },
    }

**Xapian:**

    import os
    
    HAYSTACK_CONNECTIONS = {
        'default': {
            'ENGINE': 'xapian_backend.XapianEngine',
            'PATH': os.path.join(os.path.dirname(__file__), 'xapian_index'),
        },
    }

