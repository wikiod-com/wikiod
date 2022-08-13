---
title: "Getting started with cakephp-3.0"
slug: "getting-started-with-cakephp-30"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Build up first 'Hello World!' application with CakePHP 3.x (Database tables migrations. Part 2)
You can easily `create` tables for Your database or `drop` them if You want.
If You wish to do that, You should learn how to write `Migrations` for desired database.


Migrations files must be located in `config/Migrations` folder. File names can be in next formats:

- `YYYYMMDDHHIISS_(Create|Alter|Delete)AdministratorsTable.php`
- `(1-9){1,}_(Create|Alter|Delete)AdministratorsTable.php`
  

    <?php
    use Migrations\AbstractMigration;
    use Cake\Log\Log;
    
    /**
     * Class AdministratorsTableMigration
     */
    class AdministratorsTableMigration extends AbstractMigration
    {
    
        /**
         * @var string
         */
        private $_tableName;
    
        /**
         * @var string
         */
        private $_tablePrefix;
    
        public function init()
        {
            $this->_tableName = '"Administrators"';
            $this->_tablePrefix = 'administrators';
        }
    
        public function up()
        {
            Log::info("Trying to create {$this->_tableName} table");
    
            $administratorsTable = $this->table($this->_tablePrefix);
    
            if ($administratorsTable->exists()) {
                return Log::warning("Table {$this->_tableName} already exists");
            }
    
            $administratorsTable
                ->addPrimaryKey('id')
                ->addColumn('username', 'char', [
                    'length' => 25,
                    'null' => false
                ])
                ->addColumn('password', 'char', [
                    'length' => 255,
                    'null' => false
                ])
                ->addColumn('email', 'char', [
                    'length' => 50,
                    'null' => false
                ])
                ->addColumn('first_name', 'char', [
                    'length' => 50,
                    'null' => false
                ])
                ->addColumn('last_name', 'char', [
                    'length' => 50,
                    'null' => false
                ])
                ->addColumn('avatar', 'char', [
                    'length' => 255,
                    'default' => '/img/no-avatar.png'
                ])
                ->addColumn('active', 'boolean', [
                    'default' => 0
                ])
                ->addTimestamps()
                ->create();
    
            return Log::notice("Table {$this->_tableName} has been created");
        }
    
        public function down()
        {
            if ($this->table($this->_tablePrefix)->exists()) {
                $this->table($this->_tablePrefix)->drop();
                return Log::info("Table {$this->_tableName} has been dropped");
            }
    
            return Log::warning("Table {$this->_tableName} does not exists");
        }
    
    }

If You want to run migration, You need to run next command:

`bin/cake migrations migrate` to create table(-s).

If You want to rollback:

`bin/cake migrations rollback` - will revert last migration, where `drop()` function exists

`bin/cake migrations (-t|--target) all` - will revert all migrations, where `drop()` function exists

## Installation Cakephp 3.X


Requirements:

 - PHP 5.6.0 or greater   mbstring PHP extension (default works in
   WAMP/XAMPP, if not then enable it)
 - intl PHP extension (Available in WAMP/XAMPP, you have to enable it   
   from php.ini)
 - CakePHP will run on a variety of web servers such as nginx,   
 - LightHTTPD, or Microsoft IIS.

Before starting you should make sure that your PHP version is up to date:

    php -v

Use Composer to install Cakephp 3 Framework,

Composer is officially supported method for Installation,
so download composer at, [Composer (Windows/Linux/Mac)][1] 

Run this to Install cakephp,

    php composer.phar create-project --prefer-dist cakephp/app my_app_name

Once Composer finishes downloading the application skeleton and the core CakePHP library, you should have a functioning CakePHP application installed via Composer. Be sure to keep the composer.json and composer.lock files with the rest of your source code.

Or follow this easyest way to install cakephp

Follow the bellow steps,

 1. go to this [Git Repository][1]
 2. Download the cakeDC/oven for easy installation
 3. Extract the zip file inside the LOCALHOST
 4. Give 777 permission to that folder
 5. Run file oven.php (available inside the folder)
 6. A page will open with lot of option, choose option as per your preference
 7. Click on the image and sit back. Your project will be installed in couple of minutes

  [1]: https://github.com/CakeDC/oven

execute:

    bin/cake server

By default, without any arguments provided, this will serve your application at http://localhost:8765/.

fire this at your browser, http://example.com/ or http://localhost:8765/. At this point, you’ll be presented with CakePHP’s default home, and a message that tells you the status of your current database connection and you are ready for your first application.

For more details on Installation and Setup follow, [Cakephp 3.X Installation][3]


  [1]: https://getcomposer.org/download/
  [2]: https://github.com/cakephp/cakephp/tags
  [3]: https://book.cakephp.org/3.0/en/installation.html

## Setting up Project
At first, You should create database with `mysql`, `phpMyAdmin`, `HeidiSQL` or another instruments to work with Database and let user create new one.

After that procedure, You should provide access to database for project. 

You need to go into file `/path/to/your/project/config/app.php`,then look up for `Datasources` `default`. In that array, You need to change `localhost` (on demand), `user`, `password` and `database`.

o to Your browser and refresh page. DB issue should gone and show `Green Tick` at left side.

*Done!* Your first project has been set up!

## Build up first 'Hello World!' application with CakePHP 3.x (Introduction. Part 1)
CakePHP 3.x has the ability to `bake` `controllers`, `models`, `views` and other framework defined objects.

> **Note** : If you have had some experience with the `Laravel` framework, the `artisan` component is similar to `bake`.

The `bake` application is located in `bin` folder; the following are some of the available commands:

- `bin/cake bake shell %shellName%` - to bake ShellClass
- `bin/cake bake controller %controllerName%` - to bake Controller Class
- `bin/cake bake model %modelName%` - to bake Model + Entity Class
- `bin/cake bake view %viewName%` - to bake View template
- `bin/cake bake all %className%` - to bake Controller, Model + Entity, View for developer.

> **Note** : You will not be able to `bake model` if you have no tables in your database

> **Note** : If you `bake all` components, you will get `Controllers` with pre-defined `CRUD` actions.

  

## Build up first 'Hello World!' application with CakePHP 3.x (Controllers , Response, View. Part 3)
Want to create a controller? There is 2 ways of creating it:

- Manually (You will be forced to manually create Controller file in `src/Controller`)
- Baked (Running `bin/cake bake controller %controllerName%` command from CLI)

If You want to create it manually, go to `src/Controller` folder and create file that following next pattern:

 `([A-Z]{1}[a-z]{1,})Controller.php`


In that controller, You should define `namspace`, that will be used:

    <?php
        namespace App\Controller;

Then You should name it as filename, ex. AdminiController:

    use App\Controller\AppController;

    class AdminController extends AppController{}

Inside of this class, You should create Your first method, ex. `login`:

    public function login(){}

If You will type in Your browser : `http://{{project-name}}/admin/login` it will throw an error of missing template. How to solve this problem?

You need to create under `src/Template/Admin/` `login.ctp` file. 

> **Note** : *.ctp wildcard - is Cake Template file, that is using to pass/render data You setting through controller.

In that file just type 'Hello World!' where You want, refresh page with template error and You will get Your `World`, that greets You!

> **Note** : By default, `src/Template/Layout/default.ctp` is rendering as layout, if You didn't defines one

## Installing CakePHP 3.4 on CentOS 7 with PHP 7 and SELinux enabled
This is what I did to install CakePHP on a fresh installed minimal CentOS 7

- Installed a CentOS-7-x86_64-Minimal-1611.iso in VirtualBox, two network interfaces: first NAT, second Host-Only
- set ONBOOT=yes in /etc/sysconfig/network-scripts/ifcfg-enp0s3
- reboot
- yum update
- yum install net-tools (to get ifconfig and netstat)
- yum install wget
- yum install yum-utils
- wget -q http://rpms.remirepo.net/enterprise/remi-release-7.rpm
- wget -q https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
- rpm -Uvh remi-release-7.rpm epel-release-latest-7.noarch.rpm
- yum-config-manager --enable remi-php71
- yum install php
- systemctl enable httpd
- systemctl start httpd
- firewall-cmd --permanent --zone=public --add-service=http
- firewall-cmd --permanent --zone=public --add-service=https
- firewall-cmd --reload
- yum install httpd mariadb-server mariadb php phpmyadmin
- systemctl start mariadb
- systemctl enable mariadb
- systemctl restart httpd
- yum install php-mbstring php-intl
- mysql_secure_installation
- curl -s https://getcomposer.org/installer | php
- cd /var/www/html/
- php composer.phar create-project --prefer-dist cakephp/app MyApp
- chown apache: -R MyApp/
- Create Database:

        
    # mysql -u root -p
    Enter password:
    
    mysql> CREATE DATABASE mydb;
    mysql> GRANT ALL ON mydb.* to 'myuser'@'localhost' IDENTIFIED BY '_password_';
    mysql> FLUSH PRIVILEGES;
    mysql> quit
    
- Create file /etc/httpd/conf.d/my_app.conf with content:

    
    <VirtualHost *:80>
        ServerAdmin root@localhost
        ServerName cakephp.myapp.net
        DocumentRoot /var/www/html/MyApp
        <Directory /var/www/html/MyApp>
          Allowoverride All
        </Directory>
    </VirtualHost>


- cd /var/www/html/secure_logging; chcon -Rv --type=httpd_user_content_rw_t tmp
- touch /.autorelabel; reboot

- On my host I edit /etc/hosts and enter the line (192.168.56.101 is the host-only-ip-address of my VM) 
`192.168.56.101 cakephp.myapp.net`

 

- Surf to http://cakephp.myapp.net/

- ToDo: Edit database-connection file.


