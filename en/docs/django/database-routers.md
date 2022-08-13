---
title: "Database Routers"
slug: "database-routers"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

## Adding a Database Routing file
To use multiple databases in Django, just specify each one in `settings.py`:

    DATABASES = {
        'default': {
            'NAME': 'app_data',
            'ENGINE': 'django.db.backends.postgresql',
            'USER': 'django_db_user',
            'PASSWORD': os.environ['LOCAL_DB_PASSWORD']
        },
        'users': {
            'NAME': 'remote_data',
            'ENGINE': 'django.db.backends.mysql',
            'HOST': 'remote.host.db',
            'USER': 'remote_user',
            'PASSWORD': os.environ['REMOTE_DB_PASSWORD']
        }
    }

Use a `dbrouters.py` file to specify which models should operate on which databases for each class of database operation, e.g. for remote data stored in `remote_data`, you might want the following:

    class DbRouter(object):
        """
        A router to control all database operations on models in the
        auth application.
        """
        def db_for_read(self, model, **hints):
            """
            Attempts to read remote models go to remote database.
            """
            if model._meta.app_label == 'remote':
                return 'remote_data'
            return 'app_data'
    
        def db_for_write(self, model, **hints):
            """
            Attempts to write remote models go to the remote database.
            """
            if model._meta.app_label == 'remote':
                return 'remote_data'
            return 'app_data'
    
        def allow_relation(self, obj1, obj2, **hints):
            """
            Do not allow relations involving the remote database
            """
            if obj1._meta.app_label == 'remote' or \
               obj2._meta.app_label == 'remote':
               return False
            return None
    
        def allow_migrate(self, db, app_label, model_name=None, **hints):
            """
            Do not allow migrations on the remote database
            """
            if model._meta.app_label == 'remote':
                return False
            return True

Finally, add your `dbrouter.py` to `settings.py`:

    DATABASE_ROUTERS = ['path.to.DbRouter', ]

## Specifying different databases in code
The normal `obj.save()` method will use the default database, or if a database router is used, it will use the database as specified in `db_for_write`. You can override it by using:

    obj.save(using='other_db')
    obj.delete(using='other_db')

Similarly, for reading:

    MyModel.objects.using('other_db').all()

