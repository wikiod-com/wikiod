---
title: "Run WordPress local with XAMPP"
slug: "run-wordpress-local-with-xampp"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

With XAMPP you can install a web server on your local pc.
It has an Apache web server, the database MariaDB (MySQL) and works with Perl and PHP.
After the installation you are able to run and debug e.g. content management systems like WordPress on your local pc.
You can use it with Windows, Linux, Mac OS X and Solaris.

## 1. Installing XAMPP
 1. Download the installer for the [current version][1] of XAMPP
 2. Go through the installation wizard and change nothing if you are not sure what you are changing.
 3. After the installation you see the XAMPP control panel.
Just click the start button of Apache and MySQL to run both with the basic configuration.
4. If your computer is connected to a local network, and you want access the web server from other computers too, be sure that your firewall allows it.

  [1]: https://www.apachefriends.org/index.html

## 2. Setup a database after installing XAMPP
To run WordPress on your computer you need to configurate a database first.
Be sure that Apache and MySQL are running (see [Installing XAMPP][1] step 3)

1. Start a web browser of your choice and enter "localhost" in the address.
2. Choose your preferred language and select in the category "Tools" -> `phpMyAdmin`.
3. Select the tab `Databases`.
4. Enter the name of your database (you will need this name later) below "Create database" and choose `utf8_general_ci` in the dropdown "Collation".
5. Click at your created database at the left side and select the tab `Users`.
6. Add a new user by clicking at `Add user`.
7. Enter your username, choose `local` in the "Host" dropdown (if you want access the database from other computers in the network choose `Any host`) and enter your password (you will need this later too).
8. Check if below "Database for user" `Grant all privileges on wildcard name` is checked.
9. Press `Go`.


  [1]: https://www.wikiod.com/wordpress/run-wordpress-local-with-xampp#1. Installing XAMPP

## 3. Installing WordPress after setup the database
After you installed XAMPP and setup the MySQL database you can install WordPress.

1. [Download][1] the latest version of WordPress.
2. Unzip the file and insert the folder in the root directory of your webserver (if you used the suggested path during the setup of XAMPP it is "c:\xampp\htdocs\").
3. All files which existing twice can be replaced.
4. Enter in the address field of your webbrowser "localhost/wordpress/wp-admin/install.php".
5. After choosing your language click `Continue` and then `Let's go`.
6. Replace all the default text fields with your chosen configuration (see "[Setup a database][2]"). Don't change "Database Host" and "Table Prefix", except you want to migrate your local WordPress installation online. Then keep in mind that security is important. You find more information in [Set Prefix in New WordPress Installation][3].
7. Press `Submit` and than `Run the install`.
8. Now you have to enter the name of your site, an username, password and a valid e-mail.
9. Click `Install WordPress` to finish the setup and log into your new local WordPress site.


  [1]: https://wordpress.org/download/
  [2]: https://www.wikiod.com/wordpress/run-wordpress-local-with-xampp#2. Setup a database after installing XAMPP
  [3]: https://www.wikiod.com/wordpress/secure-your-installation#Set a custom prefix for WordPress tables

