---
title: "Full Joomla installation via SSH"
slug: "full-joomla-installation-via-ssh"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Parameters
| Variable| Explanation |
| ------| ------ |
| JUSERID| Random user id |
| JUSERNAME | Specified login for admin user |
| JUSEREMAIL | Specified email for admin user |
| JUSERPASS | Specified password (will be hashed) |
| DB | Specified Database name |
| DBUSER | Specified Database user for joomla |
| DBPASS | Specified Database user password for joomla |
| DBPREFIX | Specified Database Prefix |
| JOOMLAVERSION | Joomla version to be installed |

Setting up the machine to run installation script was like:

    ## Install needed commands (apt-add-repository)
    apt-get update
    apt-get -y install software-properties-common -q
    apt-get -y install python-software-properties -q
    ### Install apache
    apt-add-repository ppa:ptn107/apache
    apt-get update
    apt-get install -y apache2
    ## Install git
    apt-get install -y git
    ## Intall mysql
    debconf-set-selections <<< 'mysql-server mysql-server/root_password password qweasd'
    debconf-set-selections <<< 'mysql-server mysql-server/root_password_again password qweasd'
    apt-get -y install mysql-server
    ## mysql_secure_installation
    ### Install PHP7
    apt-add-repository -y ppa:ondrej/php
    apt-get update
    apt-get install php7.0 php7.0-fpm php7.0-mysql php7.0-xml libapache2-mod-php7.0 php7.0-curl php7.0-sqlite php7.0-xdebug php7.0-mcrypt php7.0-gd php-mbstring -y
    a2enmod proxy_fcgi setenvif
    a2enmod mcrypt
    a2enconf php7.0-fpm
    apache2ctl restart

This will set up Apache 2.4 that is required by PHP7 

Also you may want to dirty install phpMyAdmin in Joomla subfolder for example

    wget --no-check-certificate https://files.phpmyadmin.net/phpMyAdmin/4.6.3/phpMyAdmin-4.6.3-all-languages.zip -P /var/tmp/
    unzip /var/tmp/phpMyAdmin-4.6.3-all-languages.zip -d /var/public_html/
    mv phpMyAdmin-4.6.3-all-languages phpmyadmin
    mv phpmyadmin/config.sample.inc.php phpmyadmin/config.inc.php

## Installing Joomla with SSH - full cycle
Setup variables
    
    JUSERID=$[ ( $RANDOM % 100 )  + 1 ]
    JUSERNAME="admin"
    JUSEREMAIL="admin@localhost.com"
    JUSERPASS="qweasd"
    DB="joomla_3"
    DBUSER="jdbuser"
    DBPASS="dbupass"
    DBPREFIX="prfx_"
    JOOMLAVERSION="3.6.2"

Delete initial index.html

    rm -f /var/public_html/index.html

Download Joomla version specified above

    wget https://github.com/joomla/joomla-cms/releases/download/$JOOMLAVERSION/Joomla_$JOOMLAVERSION-Stable-Full_Package.zip -P /var/tmp/

Install unzip and unzip

    apt-get install unzip
    unzip /var/tmp/Joomla_$JOOMLAVERSION-Stable-Full_Package.zip -d /var/public_html

Change folder

    cd /var/public_html

Change configuration.php values as needed (according to vars set up above)

    sed -i "s/\$user = ''/\$user = '${DBUSER}'/" installation/configuration.php-dist
    sed -i "s/\$password = ''/\$password = '${DBPASS}'/" installation/configuration.php-dist
    sed -i "s/\$db = ''/\$db = '${DB}'/" installation/configuration.php-dist
    sed -i "s/\$dbprefix = 'jos_'/\$dbprefix = '${DBPREFIX}'/" installation/configuration.php-dist
    sed -i "s/\$tmp_path = '/tmp'/\$tmp_path = '/var/public_html/tmp'" installation/configuration.php-dist
    sed -i "s/\$log_path = '/var/logs'/\$log_path = '/var/public_html/logs'/" installation/configuration.php-dist
    sed -i "s/\$cache_handler = 'file'/\$cache_handler = ''/" installation/configuration.php-dist

Move configuration.php to where it needs to be

    mv installation/configuration.php-dist configuration.php

Create DB and Joomla DB user

    echo "CREATE DATABASE ${DB}" | mysql -u root --password=qweasd
    echo "CREATE USER '${DBUSER}'@'%' IDENTIFIED BY '${DBPASS}';" | mysql -u root --password=qweasd
    echo "GRANT ALL ON ${DB}.* TO '${DBUSER}'@'%';" | mysql -u root --password=qweasd

Initialize joomla DB by sql

    sed -i "s/#__/${DBPREFIX}/" installation/sql/mysql/joomla.sql
    cat installation/sql/mysql/joomla.sql | mysql -u $DBUSER --password=$DBPASS $DB

Create Joomla Admin user

    JPASS="$(echo -n "$JUSERPASS" | md5sum | awk '{ print $1 }' )"
    echo "INSERT INTO \`${DBPREFIX}users\` (\`id\`, \`name\`, \`username\`, \`email\`, \`password\`, \`block\`, \`sendEmail\`, \`registerDate\`, \`lastvisitDate\`, \`activation\`, \`params\`, \`lastResetTime\`, \`resetCount\`, \`otpKey\`, \`otep\`, \`requireReset\`) VALUES ('${JUSERID}', 'Me', '${JUSERNAME}', '${JUSEREMAIL}', '${JPASS}', '0', '0', '0000-00-00 00:00:00.000000', '0000-00-00 00:00:00.000000', '', '', '0000-00-00 00:00:00.000000', '0', '', '', '0');" | mysql -u $DBUSER --password=$DBPASS $DB
    echo "INSERT INTO \`${DBPREFIX}user_usergroup_map\` (\`user_id\`, \`group_id\`) VALUES ('${JUSERID}', '8');" | mysql -u $DBUSER --password=$DBPASS $DB
    JUSERINC= JUSERID+1
    echo "ALTER TABLE \`${DBPREFIX}users\` auto_increment = ${JUSERINC};" | mysql -u $DBUSER --password=$DBPASS $DB

Remove installation folder

    rm -rf installation/

