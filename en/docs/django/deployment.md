---
title: "Deployment"
slug: "deployment"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Simple remote deploy fabfile.py
*Fabric is a Python (2.5-2.7) library and command-line tool for streamlining the use of SSH for application deployment or systems administration tasks. It lets you execute arbitrary Python functions via the command line.*

Install fabric via `pip install fabric`  
Create `fabfile.py` in your root directory:  

    #myproject/fabfile.py    
    from fabric.api import *
    
    @task
    def dev():
        # details of development server
        env.user = # your ssh user
        env.password = #your ssh password
        env.hosts = # your ssh hosts (list instance, with comma-separated hosts)
        env.key_filename = # pass to ssh key for github in your local keyfile

    @task
    def release():
        # details of release server
        env.user = # your ssh user
        env.password = #your ssh password
        env.hosts = # your ssh hosts (list instance, with comma-separated hosts)
        env.key_filename = # pass to ssh key for github in your local keyfile

    @task
    def run():
        with cd('path/to/your_project/'):
            with prefix('source ../env/bin/activate'): 
            # activate venv, suppose it appear in one level higher
                # pass commands one by one
                run('git pull')
                run('pip install -r requirements.txt')
                run('python manage.py migrate --noinput')
                run('python manage.py collectstatic --noinput')
                run('touch reload.txt')

To execute the file, simply use the `fab` command:

    $ fab dev run  # for release server, `fab release run`

Note: you can not configure ssh keys for github and just type login and password manually, while fabfile runs, the same with keys.  

## Running Django application with Gunicorn
 1. Install gunicorn

    `pip install gunicorn`


2. From django project folder (same folder where manage.py resides), run the following command to run current django project with gunicorn

    `gunicorn [projectname].wsgi:application -b 127.0.0.1:[port number]`

    You can use the `--env` option to set the path to load the settings

    `gunicorn --env DJANGO_SETTINGS_MODULE=[projectname].settings [projectname].wsgi`

    or run as daemon process using `-D` option



3. Upon successful start of gunicorn, the following lines will appear in console

    `Starting gunicorn 19.5.0` 
    
    `Listening at: http://127.0.0.1:[port number] ([pid])` 
    
    `....` (other additional information about gunicorn server)


## Using Heroku Django Starter Template.
If you plan to host your Django website on Heroku, you can start your project using the Heroku Django Starter Template :

    django-admin.py startproject --template=https://github.com/heroku/heroku-django-template/archive/master.zip --name=Procfile YourProjectName

It has Production-ready configuration for Static Files, Database Settings, Gunicorn, etc and Enhancements to Django's static file serving functionality via WhiteNoise. This will save your time, it's All-Ready for hosting on Heroku, Just build your website on the top of this template

To deploy this template on Heroku:

    git init
    git add -A
    git commit -m "Initial commit"
    
    heroku create
    git push heroku master
    
    heroku run python manage.py migrate

That's it!

## Django deployment instructions. Nginx + Gunicorn + Supervisor on Linux (Ubuntu)
Three basic tools.
1) nginx - free, open-source, high-performance HTTP server and reverse proxy, with high performance;
2) gunicorn - 'Green Unicorn' is a Python WSGI HTTP Server for UNIX (needed to manage your server);
3) supervisor - a client/server system that allows its users to monitor and control a number of processes on UNIX-like operating systems. Used when you app or system crashes, restarts your django / celery / celery cam, etc;

In order ot make it simple, let's assume your app is located in this directory: `/home/root/app/src/` and we're gonna use `root` user (but you should create separate user for your app). Also our virtualenvironment will be located in `/home/root/app/env/` path.



**NGINX**
-----


Let's start with nginx. If nginx is not already on machine, install it with `sudo apt-get install nginx`. Later on you have to create a new config file in your nginx directory `/etc/nginx/sites-enabled/yourapp.conf`. 
If there is a file named `default.conf` - remove it.

Bellow code to a nginx conf file, which will try to run your service with using socket file; Later on there will be a configuration of gunicorn. Socket file is used here to communicate between nginx and gunicorn. It can also be done with using ports.

    # your application name; can be whatever you want
    upstream yourappname {
        server        unix:/home/root/app/src/gunicorn.sock fail_timeout=0;
    }
    
    server {
        # root folder of your application
        root        /home/root/app/src/;
     
        listen        80;
        # server name, your main domain, all subdomains and specific subdomains
        server_name   yourdomain.com *.yourdomain.com somesubdomain.yourdomain.com
     
        charset       utf-8;
    
        client_max_body_size                        100m;
     
        # place where logs will be stored;
        # folder and files have to be already located there, nginx will not create
        access_log        /home/root/app/src/logs/nginx-access.log; 
        error_log         /home/root/app/src/logs/nginx-error.log;
        
        # this is where your app is served (gunicorn upstream above)
        location / {
            uwsgi_pass  yourappname;
            include     uwsgi_params; 
        }

        # static files folder, I assume they will be used
        location /static/ {
            alias         /home/root/app/src/static/;
        }
     
        # media files folder
        location /media/ {
            alias         /home/root/app/src/media/;
        }

    }


----------


**GUNICORN**
-----
Now our GUNICORN script, which will be responsible for running django application on server. First thing is to install gunicorn in virtual environment with `pip install gunicorn`.

    #!/bin/bash
    
    ME="root"
    DJANGODIR=/home/root/app/src # django app dir
    SOCKFILE=/home/root/app/src/gunicorn.sock # your sock file - do not create it manually
    USER=root
    GROUP=webapps
    NUM_WORKERS=3 
    DJANGO_SETTINGS_MODULE=yourapp.yoursettings
    DJANGO_WSGI_MODULE=yourapp.wsgi  
    echo "Starting $NAME as `whoami`"
     
    # Activate the virtual environment
    cd $DJANGODIR
    
    source /home/root/app/env/bin/activate
    export DJANGO_SETTINGS_MODULE=$DJANGO_SETTINGS_MODULE
    export PYTHONPATH=$DJANGODIR:$PYTHONPATH
     
    # Create the run directory if it doesn't exist
    RUNDIR=$(dirname $SOCKFILE)
    test -d $RUNDIR || mkdir -p $RUNDIR
     
    # Start your Django Gunicorn
    # Programs meant to be run under supervisor should not daemonize themselves (do not use --daemon)
    exec /home/root/app/env/bin/gunicorn ${DJANGO_WSGI_MODULE}:application \
      --name root \
      --workers $NUM_WORKERS \
      --user=$USER --group=$GROUP \
      --bind=unix:$SOCKFILE \
      --log-level=debug \
      --log-file=-

in order to be able to run gunicorn start script it has to have execution mode enabled so

`sudo chmod u+x /home/root/app/src/gunicorn_start`

now you will be able to start your gunicorn server with just using `./gunicorn_start`


----------


**SUPERVISOR**
-----
As said in the beginning, we want our application to be restarted when fails by a supervisor. If supervisor not yet on server install with `sudo apt-get install supervisor`.

At first install supervisor. 
Then create a `.conf` file in your main directory `/etc/supervisor/conf.d/your_conf_file.conf`

configuration file content:

    [program:yourappname]
    command = /home/root/app/src/gunicorn_start
    user = root
    stdout_logfile = /home/root/app/src/logs/gunicorn_supervisor.log
    redirect_stderr = true
Quick brief, `[program:youappname]` is required at the beginning, it will be our identifier. also `stdout_logfile` is a file where logs will be stored, both access and errors.

Having that done we have to tell our supervisor that we have just added new configuration file. To do it, there is different process for different Ubuntu version.

For `Ubuntu version 14.04 or lesser` than it, simply run those commands:

`sudo supervisorctl reread` -> rereads all config files inside supervisor catalog
this should print out: **yourappname: available**

`sudo supervisorctl update` -> updates supervisor to newly added config files; should print out **yourappname: added process group**

For `Ubuntu 16.04` Run:

    sudo service supervisor restart

and in order to check if your app is running correctly just run

`sudo supervisorctl status yourappname`

This should display :

`yourappname                            RUNNING    pid 18020, uptime 0:00:50`


To get live demonstration of this procedure, surf this **[video][1]**.


  [1]: https://www.youtube.com/watch?v=jN9iPaQzZbQ

## Deploying with Heroku
1. Download [Heroku Toolbelt][1].
2. Navigate to the root of the sources of your Django app. You'll need tk
3. Type `heroku create [app_name]`. If you don't give an app name, Heroku will randomly generate one for you. Your app URL will be `http://[app name].herokuapp.com`
4. Make a text file with the name `Procfile`. Don't put an extension at the end. 
   ```
   web: <bash command to start production server>
   ```
   > If you have a worker process, you can add it too. Add another line in the format: `worker-name: <bash command to start worker>`

5. Add a requirements.txt. 
  - If you are using a virtual environment, execute `pip freeze > requirements.txt`
  - Otherwise, [*get a virtual environment!*][2]. You can also manually list the Python packages you need, but that won't be covered in this tutorial.

6. **It's deployment time!** 
    1. `git push heroku master`
    > Heroku needs a git repository or a dropbox folder to do deploys. You can alternatively set up automatic reloading from a GitHub repository at [`heroku.com`][3], but we won't cover that in this tutorial. <!-- Editors - you can help with this part! -->
    2. `heroku ps:scale web=1`
    > This scales the number of web "dynos" to one. You can learn more about dynos [here.][4]
    3. `heroku open` or navigate to `http://app-name.herokuapp.com`
    > **Tip:** `heroku open` opens the URL to your heroku app in the default browser.

7. Add **add-ons**. You'll need to configure your Django app to bind with databases provided in Heroku as "add-ons". This example doesn't cover this, but another example is in the pipeline on deploying databases in Heroku.
<!-- 

 -->


  [1]: https://toolbelt.heroku.com/
  [2]: https://www.wikiod.com/django/getting-started-with-django#Virtual Environment
  [3]: http://heroku.com
  [4]: https://devcenter.heroku.com/articles/dynos

## Deploying locally without setting up apache/nginx
Recommended way of production deployment calls for using Apache/Nginx for serving the static content. Thus, when `DEBUG` is false static and media contents fail to load. However, we can load the static content in deployment without having to setup Apache/Nginx server for our app using:

    python manage.py runserver --insecure

This is only intended for local deployment(e.g LAN) and should never be used in production and is only available if the `staticfiles` app is in your project’s `INSTALLED_APPS` setting.

