---
title: "Getting started with Asterisk"
slug: "getting-started-with-asterisk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## asterisk installation on CentOS 6.X
Asterisk is an open source framework for building communications applications. You can use it for any of the following:
 

 1. IP PBX systems
 2. VoIP gateways
 3. ISDN/ 3G IVVR

Here is a brief instruction for step by step installation of **asterisk 1.8**(or you can do for latest versions) on Redhat/centos (for other linux versions the commands are similar :) ).

Step 1: Get the asterisk source code files from: [Asterisk downloads][1]

Step 2: Login as root and run the commands:
   
    yum update

    yum install joe gcc-c++ gcc-gnat gcc-g77 gcc-objc gcc autoconf automake bison flex cvs rpm-build openssl-devel SDL-devel 

    yum install redhat-rpm-config sox sox-devel curl-devel  ncurses-devel net-snmp net-snmp-libs net-snmp-utils libxml2-devel gd-devel libpng-devel doxygen

    yum install mysql-server mysql-devel // if needed 
    chkconfig --level 345 mysqld on
    service mysqld start
    yum install php php-fpm php-cli php-mysql php-gd php-imap php-ldap php-odbc php-pear php-xml php-xmlrpc php-pecl-apc php-magickwand php-magpierss php-mbstring php-mcrypt php-mssql php-shout php-snmp php-soap php-tidy //optional for web application 

    yum install bind caching-nameserver
    yum install system-config-date

    cd /path/to/asterisk/
    make clean
    ./configure

**now the environment is ready to install asterisk.**

    make menuselect // here from Add-ons check the app-mysql, cdr-mysql, res-config-mysql
    make
    make install

    make config
    chkconfig asterisk on

Step 3: Connect to asterisk

    asterisk -vvvvvvvvvvvvvvvvvvvc
    stop now
    service asterisk start

Step 4: now add the following in `/etc/asterisk/cdr.conf` file (dynamic CDR data insertion to DB)

    ;csv
    [mysql]
    usegmtime=yes    ; log date/time in GMT.  Default is "no"
    loguniqueid=yes  ; log uniqueid.  Default is "no"
    loguserfield=yes ; log user field.  Default is "no"


    and add the following in cdr_mysql.conf
    
    
    [global]
    hostname=localhost
    dbname=your_asterisk_db
    table=cdr
    password=your_ast_pass
    user=your_ast_user
    ;port=3306
    ;sock=/tmp/mysql.sock
    ;userfield=1
    

Step 5: Create your DB and create the CDR table :

`create table cdr( uniqueid varchar(32) NOT NULL default '', userfield varchar(255) NOT NULL default '', accountcode varchar(20) NOT NULL default '', src varchar(80) NOT NULL default '', dst varchar(80) NOT NULL default '', dcontext varchar(80) NOT NULL default '', clid varchar(80) NOT NULL default '', channel varchar(80) NOT NULL default '', dstchannel varchar(80) NOT NULL default '', lastapp varchar(80) NOT NULL default '', lastdata varchar(80) NOT NULL default '', calldate datetime NOT NULL default '0000-00-00 00:00:00', duration int(11) NOT NULL default '0', billsec int(11) NOT NULL default '0', disposition varchar(45) NOT NULL default '', amaflags int(11) NOT NULL default '0' );
`

Step 6: finally reboot the server

Now connect to asterisk using: `asterisk -rvvvvv`

  [1]: http://www.asterisk.org/downloads/ "Asterisk Downloads"

## Asterisk 14 installation on Ubuntu 16.04 LTS
Run the following bash script as sudo

    #!/bin/bash

    # get deps
    apt -y install build-essential libncurses5-dev libxml2-dev libsqlite3-dev libssl-dev libsrtp0-dev uuid-dev libjansson-dev

    # download
    cd /usr/src
    wget http://downloads.asterisk.org/pub/telephony/asterisk/asterisk-14-current.tar.gz
    tar -zxvf asterisk-14-current.tar.gz
    rm asterisk-14-current.tar.gz
    cd asterisk-14*

    # Install pre-requisites
    ./contrib/scripts/install_prereq install
    ./contrib/scripts/install_prereq install-unpackaged
    ./contrib/scripts/get_mp3_source.sh

    # Install
    make clean
    ./configure
    make menuselect
    make
    make install
    make samples
    make config

    # Uncomment and edit the following lines to setup a snakeoil SSL cert
    #mkdir /etc/asterisk/keys
    #cd contrib/scripts/
    # Change the following IP address to you server IP address 
    #./ast_tls_cert -C 192.168.254.2 -O "My company name" -d /etc/asterisk/keys

    # Edit the following to adjust locale of sounds
    cd /var/lib/asterisk/sounds
    wget http://downloads.asterisk.org/pub/telephony/sounds/asterisk-core-sounds-en_GB-wav-current.tar.gz
    wget http://downloads.asterisk.org/pub/telephony/sounds/asterisk-extra-sounds-en_GB-wav-current.tar.gz

    tar -xzf asterisk-core-sounds-en_GB-wav-current.tar.gz
    tar -xzf asterisk-extra-sounds-en_GB-wav-current.tar.gz
    rm *.tar.gz


