---
title: "Deploying Flask application using uWSGI web server with Nginx"
slug: "deploying-flask-application-using-uwsgi-web-server-with-nginx"
draft: false
images: []
weight: 9800
type: docs
toc: true
---

## Enable streaming from flask
Flask has that feature which lets you stream data from a view by using generators.

Let's change the `app.py` file

- add `from flask import Response`
- add `from datetime import datetime`
- add `from time import sleep`
- create a new view:
    

    @app.route("/time/")
    def time():
        def streamer():
            while True:
                yield "<p>{}</p>".format(datetime.now())
                sleep(1)
    
        return Response(streamer())

Now open your browser at `localhost/time/`. The site will load forever because nginx waits until the response is complete. In this case the response will never be complete because it will send the current date and time forever.

To prevent nginx from waiting we need to add a new line to the configuration.

Edit `/etc/nginx/sites-available/flaskconfig`

    server {
        listen 80;
        server_name localhost;
    
        location / {
            include uwsgi_params;
            uwsgi_pass unix:///tmp/flask.sock;
            uwsgi_buffering off;  # <-- this line is new
        }
    }

The line `uwsgi_buffering off;` tells nginx not to wait until a response it complete.

Restart nginx: `sudo service nginx restart` and look at `localhost/time/` again.

Now you will see that every second a new line pops up.

## Set up Flask Application, uWGSI, Nginx - Server Configurations boiler template (default, proxy and cache)
This is a porting of set up sourced from DigitalOcean's tutorial of [*How To Serve Flask Applications with uWSGI and Nginx on Ubuntu 14.04*][1]



and some useful git resources for nginx servers.

**Flask Application**

This tutorial assume you use Ubuntu.

 1. locate `var/www/` folder.
 2. Create your web app folder `mkdir myexample`
 3. `cd myexample`

*optional* You may want to set up virtual environment for deploying web applications on production server.

    sudo pip install virtualenv
to install virtual environment.

    virtualenv myexample

to set up virtual environment for your app.

    source myprojectenv/bin/activate 

to activate your environment.
Here you will install all python packages.

*end optional but recommended*


**Set up flask and gateway uWSGI**

Install flask and uSWGI gateway:

    pip install uwsgi flask

Example of flask app in myexample.py:

    from flask import Flask
    application = Flask(__name__)
    
    @application.route("/")
    def hello():
        return "<h1>Hello World</h1>"
    
    if __name__ == "__main__":
        application.run(host='0.0.0.0')

Create file to communicate between your web app and the web server: gateway interface [https://en.wikipedia.org/wiki/Web_Server_Gateway_Interface]

    nano wsgi.py

then import your webapp module and make it run from the gateway entry point.

    from myexample import application
    
    if __name__ == "__main__":
        application.run()

To test uWSGI:

    uwsgi --socket 0.0.0.0:8000 --protocol=http -w wsgi

To configure uWSGI:

 1. Create a configuration file `.ini`

    `nano myexample.ini`

 2. Basic configuration for gateway uWSGI

     
    # include header for using uwsgi
    [uwsgi]
    # point it to your python module wsgi.py
    module = wsgi
    # tell uWSGI to start a master node to serve requests
    master = true
    # spawn number of processes handling requests
    processes = 5
    # use a Unix socket to communicate with Nginx. Nginx will pass connections to uWSGI through a socket, instead of using ports. This is preferable because Nginx and uWSGI stays on the same machine.
    socket = myexample.sock
    # ensure file permission on socket to be readable and writable
    chmod-socket = 660
    # clean the socket when processes stop
    vacuum = true
    # use die-on-term to communicate with Ubuntu versions using Upstart initialisations: see:
    # http://uwsgi-docs.readthedocs.io/en/latest/Upstart.html?highlight=die%20on%20term
    die-on-term = true

*optional if you are using virtual env* You can `deactivate` your virtual environment.

**Nginx configuration**
We are gonna use nginx as:
 1. default server to pass request to the socket, using uwsgi protocol
 2. proxy-server in front of default server
 3. cache server to cache successful requests (as example, you may want to cache GET requests if your web application)

Locate your `sites-available` directory and create a configuration file for your application:

    sudo nano /etc/nginx/sites-available/myexample

Add following block, in comments what it does:

    server {
       

        # setting up default server listening to port 80
        listen 8000 default_server;
        server_name myexample.com; #you can also use your IP 
        
        # specify charset encoding, optional
        charset utf-8;

        # specify root of your folder directory
        root /var/www/myexample;

        # specify locations for your web apps.
        # here using /api endpoint as example
        location /api {
            # include parameters of wsgi.py and pass them to socket
            include uwsgi_params;
            uwsgi_pass unix:/var/www/myexample/myexample.sock;
        }
    
    }

    # Here you will specify caching zones that will be used by your virtual server
    # Cache will be stored in /tmp/nginx folder
    # ensure nginx have permissions to write and read there!
    # See also:
    # http://nginx.org/en/docs/http/ngx_http_proxy_module.html

    proxy_cache_path /tmp/nginx levels=1:2 keys_zone=my_zone:10m inactive=60m;
    proxy_cache_key "$scheme$request_method$host$request_uri";

    # set up the virtual host!
    server {
        listen   80  default_server;
        
        # Now www.example.com will listen to port 80 and pass request to http://example.com
        server_name www.example.com;

        # Why not caching responses
    
        location /api {
            # set up headers for caching
            add_header X-Proxy-Cache $upstream_cache_status;
    
            # use zone specified above
            proxy_cache my_zone;
            proxy_cache_use_stale updating;
            proxy_cache_lock on;
            
            # cache all responses ?
            # proxy_cache_valid 30d;

            # better cache only 200 responses :)
            proxy_cache_valid 200 30d;

            # ignore headers to make cache expire
            proxy_ignore_headers X-Accel-Expires Expires Cache-Control;
    
            # pass requests to default server on port 8000
            proxy_pass http://example.com:8000/api;
        }
    }

Finally, link the file to `sites-enabled` directory.
For an explanation of available and enabled sites, see answer:
[http://serverfault.com/a/527644]

    sudo ln -s /etc/nginx/sites-available/myproject /etc/nginx/sites-enabled


You are done now with nginx.
However, you may want to check out this very precious boiler template:
[https://github.com/h5bp/server-configs-nginx]

Very useful for fine tuning.

Now test Nginx:

    sudo nginx -t

Launch Nginx:

    sudo service nginx restart


**Automate Ubuntu to start uWSGI**
The last thing is to make Ubuntu start the wsgi gateway communicating with your application, otherwise you should do it manually.

1. Locate directory for initialisation scripts in Ubuntu, and create a new script:

`sudo nano /etc/init/myexample.conf`

2. Add following block, comments in line to explain what it does

    ```
    # description for the purpose of this script
    description "uWSGI server instance configured to serve myproject"
    
    # Tell to start on system runtime 2, 3, 4, 5. Stop at any other level (0,1,6). 
    # Linux run levels: [http://www.debianadmin.com/debian-and-ubuntu-linux-run-levels.html]
    start on runlevel [2345]
    stop on runlevel [!2345]

    # Set up permissions! "User" will be the username of your user account on ubuntu.
    setuid user
    # Allow www-data group to read and write from the socket file. 
    # www-data is normally the group Nginx and your web applications belong to.
    # you may have all web application projects under /var/www/ that belongs to www-data group
    setgid www-data

    # tell Ubunutu which environment to use.
    # This is the path of your virtual environment: python will be in this path if you installed virtualenv. Otherwise, use path of your python installation
    env PATH=/var/www/myexample/myexample/bin
    # then tell to Ubuntu to change and locate your web application directory
    chdir /var/www/myexample
    # finally execute initialisation script, that load your web app myexample.py
    exec uwsgi --ini myexample.ini

Now you can activate your script:
sudo start myexample


  [1]: https://www.digitalocean.com/community/tutorials/how-to-serve-flask-applications-with-uwsgi-and-nginx-on-ubuntu-14-04

## Using uWSGI to run a flask application
The built-in `werkzeug` server certainly is not suitable for running production servers. The most obvious reason is the fact that the `werkzeug` server is single-threaded and thus can only handle one request at a time.

Because of this we want to use the uWSGI Server to serve our application instead. In this example we will install uWSGI and run a simple test application with it.

__Installing uWSGI__:

    pip install uwsgi

It is as simple as that. If you are unsure about the python version your pip uses make it explicit:

    python3 -m pip install uwsgi  # for python3
    python2 -m pip install uwsgi  # for python2

Now let's create a simple test application:

_`app.py`_

    from flask import Flask
    from sys import version

    app = Flask(__name__)
    
    @app.route("/")
    def index():
        return "Hello uWSGI from python version: <br>" + version

    application = app

In flask the conventional name for the application is `app` but uWSGI looks for `application` by default. That's why we create an alias for our app in the last line.

Now it is time to run the app:

    uwsgi --wsgi-file app.py --http :5000

You should see the message "Hello uWSGI ..." by pointing your browser to `localhost:5000`

In order not to type in the full command everytime we will create a `uwsgi.ini` file to store that configuration:

_`uwsgi.ini`_

    [uwsgi]
    http = :9090
    wsgi-file = app.py
    single-interpreter = true
    enable-threads = true
    master = true

The `http` and `wsgi-file` options are the same as in the manual command. But there are three more options:


- `single-interpreter`: It is recommended to turn this on because it might interfere with the next option

- `enable-threads`: This needs to be turned on if you are using additional threads in your application. We don't use them right now but now we don't have to worry about it.

- `master`: Master mode should be enable for [various reasons](http://stackoverflow.com/questions/20197259/what-is-uwsgi-master-mode)

Now we can run the app with this command:

    uwsgi --ini uwsgi.ini
    

## Installing nginx and setting it up for uWSGI
Now we want to install nginx to serve our application.

    sudo apt-get install nginx  # on debian/ubuntu

Then we create a configuration for our website

    cd /etc/nginx/site-available  # go to the configuration for available sites
    # create a file flaskconfig with your favourite editor

_`flaskconfig`_

    server {
        listen 80;
        server_name localhost;
    
        location / {
            include uwsgi_params;
            uwsgi_pass unix:///tmp/flask.sock;
        }
    }
    
This tells nginx to listen on port 80 (default for http) and serve something at the root path (`/`). There we tell nginx to simply act as a proxy and pass every request to a socket called `flask.sock` located in `/tmp/`.

Let's enable the site:

    cd /etc/nginx/sites-enabled
    sudo ln -s ../sites-available/flaskconfig .

You might want to remove the default configuration if it is enabled:

    # inside /etc/sites-enabled
    sudo rm default

Then restart nginx:

    sudo service nginx restart

Point your browser to `localhost` and you will see an error: `502 Bad Gateway`.

This means that nginx is up and working but the socket is missing. So lets create that.

Go back to your `uwsgi.ini` file and open it. Then append these lines:

    socket = /tmp/flask.sock
    chmod-socket = 666

The first line tells uwsgi to create a socket at the given location. The socket will be used to receive requests and send back the responses. In the last line we allow other users (including nginx) to be able to read and write from that socket.

Start uwsgi again with `uwsgi --ini uwsgi.ini`. Now point your browser again to `localhost` and you will see the "Hello uWSGI" greeting again.

Note that you still can see the response on `localhost:5000` because uWSGI now serves the application via http __and__ the socket. So let's disable the http option in the ini file

    http = :5000  # <-- remove this line and restart uwsgi

Now the app can only be accessed from nginx (or reading that socket directly :) ).


