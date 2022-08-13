---
title: "Change Password"
slug: "change-password"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Change MySQL root password in Linux
To change MySQL's root user password:

**Step 1:** Stop the MySQL server.

 - in Ubuntu or Debian:  
`sudo /etc/init.d/mysql stop`
 - in CentOS, Fedora or Red Hat Enterprise Linux:  
`sudo /etc/init.d/mysqld stop`

**Step 2:**  Start the MySQL server without the privilege system.

    sudo mysqld_safe --skip-grant-tables &

or, if `mysqld_safe` is unavailable,

    sudo mysqld --skip-grant-tables &

**Step 3:** Connect to the MySQL server.

    mysql -u root

**Step 4:** Set a new password for root user.

<!-- if version [gt 5.7] -->
    FLUSH PRIVILEGES;
    ALTER USER 'root'@'localhost' IDENTIFIED BY 'new_password';
    FLUSH PRIVILEGES;
    exit;
<!-- end version if -->
<!-- if version [lte 5.7] -->
    FLUSH PRIVILEGES;
    SET PASSWORD FOR 'root'@'localhost' = PASSWORD('new_password');
    FLUSH PRIVILEGES;
    exit;
<!-- end version if -->

Note: The `ALTER USER` syntax was introduced in MySQL 5.7.6.

**Step 5:** Restart the MySQL server.

 - in Ubuntu or Debian:  
`sudo /etc/init.d/mysql stop`  
`sudo /etc/init.d/mysql start`
 - in CentOS, Fedora or Red Hat Enterprise Linux:  
`sudo /etc/init.d/mysqld stop`  
`sudo /etc/init.d/mysqld start`

## Change MySQL root password in Windows
When we want to change root password in windows, We need to follow following steps :

**Step 1 :** Start your Command Prompt by using any of below method :

Perss `Crtl+R` or Goto `Start Menu > Run` and then type `cmd` and hit enter

**Step 2 :**
Change your directory to where `MYSQL` is installed, In my case it's

    C:\> cd C:\mysql\bin

**Step 3 :** Now we need to start `mysql` command prompt

    C:\mysql\bin> mysql -u root mysql

**Step 4 :** Fire query to change `root` password

    mysql> SET PASSWORD FOR root@localhost=PASSWORD('my_new_password');



## Process


