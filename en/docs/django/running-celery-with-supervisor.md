---
title: "Running Celery with Supervisor"
slug: "running-celery-with-supervisor"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Celery + RabbitMQ with Supervisor
Celery requires a broker to handle message-passing. We use RabbitMQ because it’s easy to setup and it is well supported.

Install rabbitmq using the following command

    sudo apt-get install rabbitmq-server

Once the installation is complete, create user, add a virtual host and set permissions.

    sudo rabbitmqctl add_user myuser mypassword
    sudo rabbitmqctl add_vhost myvhost
    sudo rabbitmqctl set_user_tags myuser mytag
    sudo rabbitmqctl set_permissions -p myvhost myuser ".*" ".*" ".*"
To start the server:

    sudo rabbitmq-server


We can install celery with pip:

    pip install celery

In your Django settings.py file, your broker URL would then look something like

    BROKER_URL = 'amqp://myuser:mypassword@localhost:5672/myvhost'


Now start the celery worker
    
    celery -A your_app worker -l info

This command start a Celery worker to run any tasks defined in your django app.

Supervisor is a Python program that allows you to control and keep running any unix processes. It can also restart crashed processes. We use it to make sure Celery workers are always running.

First, Install supervisor

    sudo apt-get install supervisor

Create your_proj.conf file in your supervisor conf.d (/etc/supervisor/conf.d/your_proj.conf):

    [program:your_proj_celery]
    command=/home/your_user/your_proj/.venv/bin/celery --app=your_proj.celery:app worker -l info
    directory=/home/your_user/your_proj
    numprocs=1
    stdout_logfile=/home/your_user/your_proj/logs/celery-worker.log
    stderr_logfile=/home/your_user/your_proj/logs/low-worker.log
    autostart=true
    autorestart=true
    startsecs=10

Once our configuration file is created and saved, we can inform Supervisor of our new program through the supervisorctl command. First we tell Supervisor to look for any new or changed program configurations in the /etc/supervisor/conf.d directory with:

    sudo supervisorctl reread
  
Followed by telling it to enact any changes with:

    sudo supervisorctl update

Once our programs are running, there will undoubtedly be a time when we want to stop, restart, or see their status.
    
    sudo supervisorctl status

For restart your celery instance:
    
    sudo supervisorctl restart your_proj_celery



## Celery Configuration
## CELERY ##

1. Installation - `pip install django-celery`
2. Add 
3. Basic project structure.

        - src/
          - bin/celery_worker_start # will be explained later on
          - logs/celery_worker.log
          - stack/__init __.py
          - stack/celery.py
          - stack/settings.py
          - stack/urls.py
          - manage.py
4. Add `celery.py` file to your `stack/stack/` folder. 

        from __future__ import absolute_import
        import os
        from celery import Celery
        os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stack.settings')
        from django.conf import settings  # noqa
        app = Celery('stack')
        app.config_from_object('django.conf:settings')
        app.autodiscover_tasks(lambda: settings.INSTALLED_APPS)
5. to your `stack/stack/__init__.py` add  following code:

        from __future__ import absolute_import
        from .celery import app as celery_app  # noqa

6. Create a task and mark it for example as `@shared_task()`

        @shared_task()
        def add(x, y):
            print("x*y={}".format(x*y))
7. Running celery worker "by hand":
    
    `celery -A stack worker -l info` if you also want to add


## Running Supervisor
1. Create a script to start celery worker. Insert your script within your app. For example: `stack/bin/celery_worker_start`

        #!/bin/bash
        
        NAME="StackOverflow Project - celery_worker_start"
        
        PROJECT_DIR=/home/stackoverflow/apps/proj/proj/
        ENV_DIR=/home/stackoverflow/apps/proj/env/
        
        echo "Starting $NAME as `whoami`"
        
        # Activate the virtual environment
        cd "${PROJECT_DIR}"
        
        if [ -d "${ENV_DIR}" ]
        then
            . "${ENV_DIR}bin/activate"
        fi
        
        celery -A stack --loglevel='INFO'

2. Add execution rights to your newly created script:
    
    `chmod u+x bin/celery_worker_start`

3. Install supervisor (skip this test if supervisor already installed)

    `apt-get install supervisor`

4. Add config file for your supervisor in order to start you celery. Place it in `/etc/supervisor/conf.d/stack_supervisor.conf`

        [program:stack-celery-worker]
        command = /home/stackoverflow/apps/stack/src/bin/celery_worker_start
        user = polsha
        stdout_logfile = /home/stackoverflow/apps/stack/src/logs/celery_worker.log
        redirect_stderr = true
        environment = LANG = en_US.UTF-8,LC_ALL = en_US.UTF-8
        numprocs = 1
        autostart = true
        autorestart = true
        startsecs = 10
        stopwaitsecs = 600
        priority = 998
    
5. Reread and update supervisor

        sudo supervisorctl reread
            stack-celery-worker: available
        sudo supervisorctl update
            stack-celery-worker: added process group
6. Basic commands

        sudo supervisorctl status stack-celery-worker                       
            stack-celery-worker      RUNNING    pid 18020, uptime 0:00:50
        sudo supervisorctl stop stack-celery-worker  
            stack-celery-worker: stopped
        sudo supervisorctl start stack-celery-worker                        
            stack-celery-worker: started
        sudo supervisorctl restart stack-celery-worker 
            stack-celery-worker: stopped
            stack-celery-worker: started
        

