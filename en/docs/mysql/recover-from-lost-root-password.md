---
title: "Recover from lost root password"
slug: "recover-from-lost-root-password"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Set root password, enable root user for socket and http access
Solves problem of: access denied for user root using password YES
Stop mySQL:

    sudo systemctl stop mysql

Restart mySQL, skipping grant tables:

    sudo mysqld_safe --skip-grant-tables

Login:

    mysql -u root
In SQL shell, look if users exist:

    select User, password,plugin FROM mysql.user ;
Update the users (plugin null enables for all plugins): 

    update mysql.user set password=PASSWORD('mypassword'), plugin = NULL WHERE User = 'root';
    exit;

In Unix shell stop mySQL without grant tables, then restart with grant tables:

    sudo service mysql stop
    sudo service mysql start

