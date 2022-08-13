---
title: "Getting started with openerp"
slug: "getting-started-with-openerp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installating from source in a Debian/Ubuntu system
Detailed instructions on getting openerp set up or installed in Debian/Ubuntu.

To install from source code, we need Python 2.7, Git and a PostgreSQL database:

```sh
$ sudo apt-get install git python-pip python2.7-dev -y
$ sudo apt-get install postgresql -y
$ sudo su -c "createuser -s $(whoami)" postgres
```

Next we need some system dependencies, required by the Python packages used by Odoo:

```sh
$ sudo apt-get install libxml2-dev libxslt1-dev \
    libevent-dev libsasl2-dev libpq-dev \
    libpng12-dev libjpeg-dev \
    node-less node-clean-css -y
```

The Python dependencies are declared in the `requirements.txt` file:

```sh
$ wget https://raw.githubusercontent.com/odoo/odoo/master/requirements.txt
$ sudo -H pip install --upgrade pip
$ sudo -H pip install -r requirements.txt
```

Now you can get the source code from GitHub, and start create your first Odoo instance:

```
$ git clone https://github.com/odoo/odoo.git -b 9.0 --depth=1
$ ./odoo/odoo.py -d myodoo 
```



## Odoo on AWS
## Note: 
### Loading Odoo onto an AWS EC2 Container requires an AWS account

Loading Odoo onto an AWS EC2 Instance can be done with one-click, simply go 
[here](https://aws.amazon.com/marketplace/pp/B00QBWCCQ6/ref_=_mkt_ste_menu?nc2=h_l3_ba) or search for "Odoo AWS" in Google.

This may take some time, but once it's ready you'll need to do two things: 
1. Get your password
2. Find your username

## **Getting your Password**
In your amazonaws EC2 dashboard, select your new Odoo instance and click the 'Actions' button at the top. Choose: **Actions > Instance Settings > Get System Log**. Scroll through the system log until you find the part which tells you **Setting Bitnami application password to xxxxxxxxx**. It will look similar to this:
 
[![enter image description here][1]][1]

### Be sure to change your password once you've logged into the application portal
## **Getting your Username**
Your username can be found from the page which you downloaded the Odoo Machine Image in the section titled **Usage Instructions**. However, the default email is commonly **user@example.com or admin@example.com**

## **After Installation**
Once Odoo is installed it will also add it's own security group to your account and apply that security group to itself. By default, this security group allows the following inbound traffic

| Ports| Protocol| Source
| ------ | ------ | ------|
| 80 | tcp | 0.0.0.0/0
| 22 | tcp | 0.0.0.0/0
| 443| tcp | 0.0.0.0/0

This will allow for basic HTTP and HTTPS as well as SSH and SFTP connections. If you wish to alter these settings you can click on the security group and edit the rules on the **inboud** tag OR you can add a new security group by clicking **Actions > Networking > Change Security Groups** in the EC2 Dashboard

### link for odoo on aws: https://aws.amazon.com/marketplace/pp/B00QBWCCQ6/ref_=_mkt_ste_menu?nc2=h_l3_ba




  [1]: https://i.stack.imgur.com/k8tVd.jpg

## Manage Server Instances
Once we have Odoo installed, we need to create a server instance.
A server instance is an Odoo service listening on a specific port, 8060, by default, and using a database to store data.

The minimal command to start an Odoo instance using the `mydb` database:

```
$ ./odoo.py -d mydb
```

If the database does not exist yet, Odoo will automatically try to create it, and then automatically creates all the database tables it needs.
If the user starting the service does not have privileges to create databases (a security best practice), we should use the PostgreSQL `createdb` command to create it. beforehand.

Below is an example of the most common options used when working with Odoo:

```
$ ./odoo.py -d mydb --db-filter=^mydb$ --addons-path=./addons,../myaddons --xmlrpc-port=8080
```

## Odoo's Nightly repositories
Odoo provides official installers, for various OS types and Openerp/Odoo versions, at the  [**Nightly repository**][1]

# Option 1: manual install from prebuilt installer package (Windows, Linux):
Download installer/package of version of your choice for your system ( Windows (.exe), Debian/Ubuntu (.deb), Redhat/CentOS(.rpm) ) from the **Odoo's Nightly Repository**: http://nightly.odoo.com/ and install it.

# Option 2: Set up repository entry to get further updates using standard OS package management system (Linux only):    
- Setup of the Debian nightly repository [details][2] 

```
    wget -O - https://nightly.odoo.com/odoo.key | apt-key add -
    echo "deb http://nightly.odoo.com/8.0/nightly/deb/ ./" >> /etc/apt/sources.list
    apt-get update && apt-get install odoo
```

- Setup of the RedHat nightly repository [details][3]
```
    yum-config-manager --add-repo=https://nightly.odoo.com/8.0/nightly/rpm/odoo.repo
    yum update && yum install odoo
```

# Note:   
Examples are shown Odoo version 8.0 repository links, adapt these links to install different version by changing `/8.0/` in the above repository URLs to `/7.0/` for Openerp v7 or `/9.0/` for Odoo v9, according your version of choice


  [1]: http://nightly.odoo.com/
  [2]: https://www.odoo.com/documentation/8.0/setup/install.html#deb
  [3]: https://www.odoo.com/documentation/8.0/setup/install.html#rpm

