---
title: "Multiple DB Connections in Laravel"
slug: "multiple-db-connections-in-laravel"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Initial Steps
Multiple database connections, of any type, can be defined inside the database configuration file (likely `app/config/database.php`). For instance, to pull data from 2 MySQL databases define them both separately:

    <?php
    return array(
    
        'default' => 'mysql',
    
        'connections' => array(
    
            # Our primary database connection
            'mysql' => array(
                'driver'    => 'mysql',
                'host'      => 'host1',
                'database'  => 'database1',
                'username'  => 'user1',
                'password'  => 'pass1'
                'charset'   => 'utf8',
                'collation' => 'utf8_unicode_ci',
                'prefix'    => '',
            ),
    
            # Our secondary database connection
            'mysql2' => array(
                'driver'    => 'mysql',
                'host'      => 'host2',
                'database'  => 'database2',
                'username'  => 'user2',
                'password'  => 'pass2'
                'charset'   => 'utf8',
                'collation' => 'utf8_unicode_ci',
                'prefix'    => '',
            ),
        ),
    );

The default connection is still set to `mysql`. This means unless otherwise specified, the application uses the `mysql` connection.

## Using Eloquent
There are multiple ways to define [which connection][1] to use in the Eloquent models. One way is to set the [$connection][2] variable in the model:


  [1]: https://laravel.com/docs/5.4/eloquent#basic-usage
  [2]: https://github.com/laravel/framework/blob/master/src/Illuminate/Database/Eloquent/Model.php#L28

    <?php
    
    class SomeModel extends Eloquent {
    
        protected $connection = 'mysql2';
    
    }

The connection can also be defined at runtime via the `setConnection` method.

    <?php
    
    class SomeController extends BaseController {
    
        public function someMethod()
        {
            $someModel = new SomeModel;
    
            $someModel->setConnection('mysql2');
    
            $something = $someModel->find(1);
    
            return $something;
        }
    }

## Using Schema builder
Within the Schema Builder, use the Schema facade with any connection. Run the `connection()` method to specify which connection to use:

    Schema::connection('mysql2')->create('some_table', function($table)
    {
        $table->increments('id'):
    });

## Using DB query builder
Similar to Schema Builder, [define a connection][1] on the Query Builder:


  [1]: https://laravel.com/docs/5.3/database#accessing-connections

    $users = DB::connection('mysql2')->select(...);

## From Laravel Documentation
Each individual connection can be accessed via the connection method on the `DB` facade, even when there are multiple connections defined. The `name` passed to the `connection` method should correspond to one of the connections listed in the `config/database.php` configuration file:  

    $users = DB::connection('foo')->select(...);

The raw can also be accessed, underlying PDO instance using the getPdo method on a connection instance:  

    $pdo = DB::connection()->getPdo();

https://laravel.com/docs/5.4/database#using-multiple-database-connections

