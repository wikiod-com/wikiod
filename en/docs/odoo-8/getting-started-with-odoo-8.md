---
title: "Getting started with odoo-8"
slug: "getting-started-with-odoo-8"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What is Odoo?
Odoo (formerly known as OpenERP and before that, TinyERP) is a suite of open core enterprise management applications. Targeting companies of all sizes, the application suite covers all business needs, from Website/Ecommerce down to manufacturing, inventory and accounting, all seamlessly integrated. It is the first time ever a software editor managed to reach such a functional coverage. Odoo is the most installed business software in the world. Odoo is used by 2,000,000+ users worldwide ranging from very small companies (1 user) to very large ones (300,000 users).

The source code for the OpenObject framework and core ERP (enterprise resource planning) modules is curated by the Belgium-based Odoo S.A. Additionally, customized programming, support, and other services are provided by an active global community and a network of 500 official partners. The main Odoo components are the OpenObject framework, about 30 core modules (also called official modules) and more than 3000 community modules

Odoo has been used as a component of university courses. A study on experiential learning suggested that OpenERP provides a suitable alternative to proprietary systems to supplement teaching.

Several books have been written about Odoo, some covering specific areas such as accounting or development

Odoo has received awards including Trends Gazelle and BOSSIE Awards three years in a row.

It uses Python scripting and PostgreSQL as it's database. Its community edition is supplemented with an Enterprise edition @ USD 240/- per user per year and a commercially supported online edition.The development repository is on GitHub.

In 2013, the not-for-profit Odoo Community Association was formed to ensure the ongoing promotion and maintenance of the Odoo community versions and modules to supplement the work of Odoo S.A. This organisation has over 150 members who are a mix of individuals and organisations.

## Setup
Odoo can be installed in three different ways:

 1. Packaged installers (easiest, less flexible)
 2. Source install (takes sometime to setup, very flexible)
 3. An official docker image from [docker.com][1]

Official packages with all relevant dependency requirements are available on [odoo.com][2].

**Windows**

Download and run the [installer][3].

**Note:** On Windows 8 you may see a warning titled "Windows protected your PC". Click More Info then Run it anyway. Accept the UAC prompt and go through the various installation steps. Odoo will automatically be started at the end of the installation.

## Configuration: ##

The configuration file can be found at %PROGRAMFILES%\Odoo 8.0-id\server\openerp-server.conf. (id is your system username)

The configuration file can be edited to connect to a remote Postgresql, edit file locations or set a dbfilter. To reload the configuration file, restart the Odoo service via Services ‣ odoo server.

**Linux**

**Debian based distributions**

To install Odoo 8.0 on Debian-based distribution, execute the following commands as root:

    # wget -O - https://nightly.odoo.com/odoo.key | apt-key add -
    # echo "deb http://nightly.odoo.com/8.0/nightly/deb/ ./" >> /etc/apt/sources.list
    # apt-get update && apt-get install odoo

This will automatically install all dependencies, install Odoo itself as a daemon and automatically start it.



**Note that**

to print PDF reports, you must install wkhtmltopdf yourself: the version of wkhtmltopdf available in debian repositories does not support headers and footers so it can not be installed automatically. The recommended version is 0.12.1 and is available on the wkhtmltopdf download page, in the archive section. As there is no official release for Debian Jessie, you can find the package on http://nightly.odoo.com/extra/.
or you can download and install it from wkhtmltopdf's download page like this

    # wget https://bitbucket.org/wkhtmltopdf/wkhtmltopdf/downloads/{path to correct distro and system architecture}
    # sudo dpkg -i {.deb package}
    # sudo cp /usr/local/bin/wkhtmlto* /usr/bin/

The configuration file can be found at /etc/odoo/openerp-server.conf

When the configuration file is edited, Odoo must be restarted using service:

$ sudo service odoo restart
Restarting odoo: ok



**RPM based distributions**

With RHEL-based distributions (RHEL, CentOS, Scientific Linux), EPEL must be added to the distribution's repositories for all of Odoo's dependencies to be available. For CentOS:

    $ sudo yum install -y epel-release

For other RHEL-based distribution, see the EPEL documentation.

Below are the installation steps.

    $ sudo yum install -y postgresql-server
    $ sudo postgresql-setup initdb
    $ sudo systemctl enable postgresql
    $ sudo systemctl start postgresql
    $ sudo yum-config-manager --add-repo=https://nightly.odoo.com/8.0/nightly/rpm/odoo.repo
    $ sudo yum install -y odoo
    $ sudo systemctl enable odoo
    $ sudo systemctl start odoo
    
    

**Note that**

To print PDF reports, you must install wkhtmltopdf yourself: the version of wkhtmltopdf available in Fedora/CentOS repositories does not support headers and footers so it can not be installed automatically. Use the version available on the wkhtmltopdf download page.
Configuration, similar to debian it can be installed with

    wget https://bitbucket.org/wkhtmltopdf/wkhtmltopdf/downloads/{path to correct distro and system architecture}
    sudo rpm -i  {.rpm package}
    sudo cp /usr/local/bin/wkhtmlto* /usr/bin/

The configuration file can be found at /etc/odoo/openerp-server.conf

When the configuration file is edited, Odoo must be restarted via Systemd:

    $ sudo systemctl restart odoo
    
    

**Source Install**

Odoo zip can be downloaded from https://nightly.odoo.com/8.0/nightly/src/odoo_8.0.latest.zip, the zip file then needs to be uncompressed to use its content

Git allows simpler update and easier switching between differents versions of Odoo. It also simplifies maintaining non-module patches and contributions. The primary drawback of git is that it is significantly larger than a tarball as it contains the entire history of the Odoo project.

The git repository is `https://github.com/odoo/odoo.git.`

Then you can clone the repository with

    $ git clone https://github.com/odoo/odoo.git
    

**Installing dependencies**

Source installation requires manually installing dependencies:

**Python 2.7.**
on Linux and OS X, included by default

on Windows, use the official Python 2.7.9 installer.

if Python is already installed, make sure it is 2.7.9, previous versions are less convenient and 3.x versions are not compatible with Odoo


**configuring PostgreSQL**

After installation you will need to create a postgres user: by default the only user is postgres, and Odoo forbids connecting as postgres.

on Linux, use your distribution's package, then create a postgres user named like your login:

    $ sudo su - postgres -c "createuser -s $USER"

Because the role login is the same as your unix login unix sockets can be use without a password.
on OS X, postgres.app is the simplest way to get started, then create a postgres user as on Linux

on Windows, use PostgreSQL for windows then
add PostgreSQL's bin directory (default: C:\Program Files\PostgreSQL\9.4\bin) to your PATH

create a postgres user with a password using the pg admin gui: open pgAdminIII, double-click the server to create a connection, select Edit ‣ New Object ‣ New Login Role, enter the usename in the Role Name field (e.g. odoo), then open the Definition tab and enter the password (e.g. odoo), then click OK.

The user and password must be passed to Odoo using either the -w and -r options or the configuration file

Python dependencies listed in the requirements.txt file.

on Linux, python dependencies may be installable with the system's package manager or using pip.

For libraries using native code (Pillow, lxml, greenlet, gevent, psycopg2, ldap) it may be necessary to install development tools and native dependencies before pip is able to install the dependencies themselves. These are available in -dev or -devel packages for Python, Postgres, libxml2, libxslt, libevent, libsasl2 and libldap2. Then the Python dependecies can themselves be installed:

    $ pip install -r requirements.txt

On OS X, you will need to install the Command Line Tools (xcode-select --install) then download and install a package manager of your choice (homebrew, macports) to install non-Python dependencies. pip can then be used to install the Python dependencies as on Linux:

    $ pip install -r requirements.txt

on Windows you need to install some of the dependencies manually, tweak the requirements.txt file, then run pip to install the remaning ones.

    Install psycopg using the installer here http://www.stickpeople.com/projects/python/win-psycopg/

Then edit the requirements.txt file:
remove psycopg2 as you already have it.
remove the optional python-ldap, gevent and psutil because they require compilation.
add pypiwin32 because it's needed under windows.

Then use pip to install the dependencies using the following command from a cmd.exe prompt (replace \YourOdooPath by the actual path where you downloaded Odoo):

    C:\> cd \YourOdooPath
    C:\YourOdooPath> C:\Python27\Scripts\pip.exe install -r requirements.txt

Less CSS via nodejs

on Linux, use your distribution's package manager to install nodejs and npm.



**Note that**

In debian wheezy and Ubuntu 13.10 and before you need to install nodejs manually:

    $ wget -qO- https://deb.nodesource.com/setup | bash -
    $ apt-get install -y nodejs
    
    

In later debian versions (>jessie) and ubuntu (>14.04) you may need to add a symlink as npm packages call node but debian calls the binary nodejs

    $ apt-get install -y npm
    $ sudo ln -s /usr/bin/nodejs /usr/bin/node

    Once npm is installed, use it to install less and less-plugin-clean-css:

    $ sudo npm install -g less less-plugin-clean-css

    on OS X, install nodejs via your preferred package manager (homebrew, macports) then install less and less-plugin-clean-css:

    $ sudo npm install -g less less-plugin-clean-css

on Windows, install `nodejs`, reboot (to update the PATH) and install `less` and `less-plugin-clean-css`:

    C:\> npm install -g less less-plugin-clean-css



**Running Odoo**

Once all dependencies are set up, Odoo can be launched by running odoo.py.

Configuration can be provided either through command-line arguments or through a configuration file.

Common necessary configurations are:

    PostgreSQL host, port, user and password.

Odoo has no defaults beyond psycopg2's defaults: connects over a UNIX socket on port 5432 with the current user and no password. By default this should work on Linux and OS X, but it will not work on windows as it does not support UNIX sockets.
    Custom addons path beyond the defaults, to load your own modules

Under Windows a typical way to execute odoo would be:

    C:\YourOdooPath> python odoo.py -w odoo -r odoo --addons-path=addons,../mymodules --db-filter=mydb$

Where odoo, odoo are the postgresql login and password, ../mymodules a directory with additional addons and mydb the default db to serve on localhost:8069

Under *nix systems a typical way to execute odoo would be:

    $ ./odoo.py --addons-path=addons,../mymodules --db-filter=mydb$Packaged installers


  [1]: https://hub.docker.com/_/odoo/
  [2]: https://nightly.odoo.com
  [3]: https://nightly.odoo.com/8.0/nightly/exe/odoo_8.0.latest.exe

