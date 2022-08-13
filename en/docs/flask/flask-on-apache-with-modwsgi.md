---
title: "Flask on Apache with mod_wsgi"
slug: "flask-on-apache-with-mod_wsgi"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## WSGI Application wrapper
Many Flask applications are developed in a *virtualenv* to keep dependencies for each application separate from the system-wide Python installation. Make sure that *mod-wsgi* is installed in your *virtualenv*:

    pip install mod-wsgi

Then create a wsgi wrapper for your Flask application. Usually it's kept in the root directory of your application.


**my-application.wsgi**

    activate_this = '/path/to/my-application/venv/bin/activate_this.py'
    execfile(activate_this, dict(__file__=activate_this))
    import sys
    sys.path.insert(0, '/path/to/my-application')

    from app import app as application

This wrapper activates the virtual environment and all of its installed modules and dependencies when run from Apache, and makes sure the application path is first in the search paths. By convention, WSGI application objects are called `application`. 

## Apache sites-enabled configuration for WSGI
The advantage of using Apache over the builtin werkzeug server is that Apache is multi-threaded, meaning that multiple connections to the application can be made simultaneously. This is especially useful in applications that make use of *XmlHttpRequest* (AJAX) on the front-end.

**/etc/apache2/sites-available/050-my-application.conf** (or default apache configuration if not hosted on a shared webserver)

    <VirtualHost *:80>
            ServerName my-application.org
    
            ServerAdmin admin@my-application.org

            # Must be set, but can be anything unless you want to serve static files
            DocumentRoot /var/www/html
    
            # Logs for your application will go to the directory as specified:
    
            ErrorLog ${APACHE_LOG_DIR}/error.log
            CustomLog ${APACHE_LOG_DIR}/access.log combined
    
            # WSGI applications run as a daemon process, and need a specified user, group
            # and an allocated number of thread workers. This will determine the number
            # of simultaneous connections available.
            WSGIDaemonProcess my-application user=username group=username threads=12
            
            # The WSGIScriptAlias should redirect / to your application wrapper:
            WSGIScriptAlias / /path/to/my-application/my-application.wsgi
            # and set up Directory access permissions for the application:
            <Directory /path/to/my-application>
                    WSGIProcessGroup my-application
                    WSGIApplicationGroup %{GLOBAL}
                    
                    AllowOverride none
                    Require all granted
            </Directory>
    </VirtualHost>



