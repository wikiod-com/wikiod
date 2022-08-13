---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Logging to Syslog service
It is possible to configure Django to output log to a local or remote syslog service. This configuration uses the python builtin [SysLogHandler][1].

    from logging.handlers import SysLogHandler
    LOGGING = {
        'version': 1,
        'disable_existing_loggers': True,
        'formatters': {
            'standard': {
                'format' : "[YOUR PROJECT NAME] [%(asctime)s] %(levelname)s [%(name)s:%(lineno)s] %(message)s",
                'datefmt' : "%d/%b/%Y %H:%M:%S"
            }
        },
        'handlers': {
            'console': {
                'class': 'logging.StreamHandler',
            },
            'syslog': {
                'class': 'logging.handlers.SysLogHandler',
                'formatter': 'standard',
                'facility': 'user',
                # uncomment next line if rsyslog works with unix socket only (UDP reception disabled)
                #'address': '/dev/log'
            }
        },
        'loggers': {
            'django':{
                'handlers': ['syslog'],
                'level': 'INFO',
                'disabled': False,
                'propagate': True
            }
        }
    }
    
    # loggers for my apps, uses INSTALLED_APPS in settings
    # each app must have a configured logger
    # level can be changed as desired: DEBUG, INFO, WARNING...
    MY_LOGGERS = {}
    for app in INSTALLED_APPS:
        MY_LOGGERS[app] = {
            'handlers': ['syslog'],
            'level': 'DEBUG',
            'propagate': True,
        }
    LOGGING['loggers'].update(MY_LOGGERS)


  [1]: https://docs.python.org/3.6/library/logging.handlers.html#sysloghandler

## Django basic logging configuration
Internally, Django uses the Python logging system. There is many way to configure the logging of a project. Here is a base:

    LOGGING = {
        'version': 1,
        'disable_existing_loggers': False,
        'formatters': {
            'default': {
                'format': "[%(asctime)s] %(levelname)s [%(name)s:%(lineno)s] %(message)s",
                'datefmt': "%Y-%m-%d %H:%M:%S"
            },
        },
        'handlers': {
            'console': {
                'level': 'INFO',
                'class': 'logging.StreamHandler',
                'formatter': 'default'
            },
        },
        'loggers': {
            'django': {
                'handlers': ['console'],
                'propagate': True,
                'level': 'INFO',
            },
        }
    }

**Formatters**

It can be used to configure logs appearence when they are printed to output. You can define many formatters by setting a key string to each different formatter. A formatter is then used when declaring a handler.

**Handlers**

Can be used to configure where the logs will be printed. In the example above, they are sent to stdout and stderr. There is various handler classes:

    'rotated_logs': {
        'class': 'logging.handlers.RotatingFileHandler',
        'filename': '/var/log/my_project.log',
        'maxBytes': 1024 * 1024 * 5,  # 5 MB
        'backupCount': 5,
        'formatter': 'default'
        'level': 'DEBUG',
    },

This will produce logs in file tergeted by `filename`. In this example, a new log file will be created when the current reach the size of 5 MB (the old one is renamed to my_project.log.1) and the latest 5 files will be kept for archive.

    'mail_admins': {
        'level': 'ERROR',
        'class': 'django.utils.log.AdminEmailHandler'
    },

This will send each log by eamil to users specified in `ADMINS` setting variable. The level is set to `ERROR`, so only logs with level `ERROR` will be sent by e-mail. This is extremely useful to stay informed on potential errors 50x on a production server.

Other handlers can be used with Django. For a full list, please read the corresponding [documentation][1]. Like formatters, you can define many handlers in a same project, setting for each a different key string. Each handler can be used in a specific logger.

**Loggers**

In `LOGGING`, the last part configure for each module the minimal logging level, the handlers(s) to use, etc.

  [1]: https://docs.python.org/3.6/library/logging.handlers.html

