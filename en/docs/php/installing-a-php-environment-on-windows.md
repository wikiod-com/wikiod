---
title: "Installing a PHP environment on Windows"
slug: "installing-a-php-environment-on-windows"
draft: false
images: []
weight: 9894
type: docs
toc: true
---

HTTP services normally run on port 80, but if you have some application installed like Skype which also utilizes port 80 then it won't start. In that case you need to change either its port or the port of the conflicting application. When done, restart the HTTP service.

## Download and Install XAMPP
# What is XAMPP?

XAMPP is the most popular PHP development environment. XAMPP is a completely free, open-source and easy to install Apache distribution containing MariaDB, PHP, and Perl.

# Where should I download it from?

Download appropriate stable XAMPP version from [their download page](//www.apachefriends.org/download.html). Choose the download based on the type of OS (32 or 64bit and OS version) and the PHP version it has to support.

Current latest being [XAMPP for Windows 7.0.8 / PHP 7.0.8](//www.apachefriends.org/xampp-files/7.0.8/xampp-win32-7.0.8-0-VC14-installer.exe).

Or you can follow this:

XAMPP for Windows exists in three different flavors:

 - [Installer][1] (Probably `.exe format` the easiest way to install XAMPP)
 - [ZIP][2] (For purists: XAMPP as ordinary ZIP `.zip format` archive)
 - [7zip:][3] (For purists with low bandwidth: XAMPP as 7zip `.7zip
   format` archive)

# How to install and where should I place my PHP/html files?

## Install with the provided installer

1. Execute the XAMPP server installer by double clicking the downloaded `.exe`.

## Install from the ZIP

1. Unzip the zip archives into the folder of your choice.
2. XAMPP is extracting to the subdirectory `C:\xampp` below the selected target directory.
3. Now start the file `setup_xampp.bat`, to adjust the XAMPP configuration to your system.

> **Note:** If you choose a root directory `C:\` as target, you must not start `setup_xampp.bat`.

## Post-Install

Use the "XAMPP Control Panel" for additional tasks, like starting/stopping Apache, MySQL, FileZilla and Mercury or installing these as services.

# File handling

The installation is a straight forward process and once the installation is complete you may add html/php files to be hosted on the server in `XAMPP-root/htdocs/`. Then start the server and open `http://localhost/file.php` on a browser to view the page.

> **Note:** Default XAMPP root in Windows is `C:/xampp/htdocs/`

Type in one of the following URLs in your favourite web browser:

<!-- language: lang-none -->

    http://localhost/
    http://127.0.0.1/

Now you should see the XAMPP start page.

[![enter image description here][4]][4]

  [1]: https://sourceforge.net/projects/xampp/files/XAMPP%20Windows/7.0.8/xampp-portable-win32-7.0.8-0-VC14-installer.exe/download
  [2]: https://sourceforge.net/projects/xampp/files/XAMPP%20Windows/7.0.8/xampp-portable-win32-7.0.8-0-VC14.zip/download
  [3]: https://sourceforge.net/projects/xampp/files/XAMPP%20Windows/7.0.8/xampp-portable-win32-7.0.8-0-VC14.7z/download
  [4]: http://i.stack.imgur.com/8gS2c.jpg

## Download, Install and use WAMP
WampServer is a Windows web development environment. It allows you to create web applications with Apache2, PHP and a MySQL database. Alongside, PhpMyAdmin allows you to manage easily your databases.

WampServer is available for free (under GPML license) in two distinct versions : 32 and 64 bits. Wampserver 2.5 is not compatible with Windows XP, neither with SP3, nor Windows Server 2003. Older WampServer versions are available on [SourceForge](https://sourceforge.net/projects/wampserver/files/).

 WampServer versions:

- [WampServer (64 BITS) 3](https://sourceforge.net/projects/wampserver/files/WampServer%203/WampServer%203.0.0/wampserver3.0.4_x64_apache2.4.18_mysql5.7.11_php5.6.19-7.0.4.exe/download)    
- [WampServer (32 BITS) 3](https://sourceforge.net/projects/wampserver/files/WampServer%203/WampServer%203.0.0/wampserver3.0.4_x86_apache2.4.18_mysql5.7.11_php5.6.19-7.0.4.exe/download)

Providing currently:

- Apache: 2.4.18
- MySQL: 5.7.11
- PHP: 5.6.19 & 7.0.4

Installation is simple, just execute the installer, choose the location and finish it.

Once that is done, you may start WampServer.
Then it starts in the system tray (taskbar), initially red in color and then turns green once the server is up.

You may goto a browser and type **localhost or 127.0.0.1** to get the index page of WAMP.
You may work on PHP locally from now by storing the files in `<PATH_TO_WAMP>/www/<php_or_html_file>` and check the result on `http://localhost/<php_or_html_file_name>`

## Install PHP and use it with IIS


