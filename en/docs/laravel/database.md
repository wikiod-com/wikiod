---
title: "Database"
slug: "database"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Multiple database connections
<!-- language-all: lang-php -->
Laravel allows user work on multiple database connections. If you need to connect to multiple databases and make them work together, you are beware of the connection setup.

You also allow using different types of database in the same application if you required.

**Default connection**
In `config/database.php`, you can see the configuration item call:

    'default' => env('DB_CONNECTION', 'mysql'),

This name references the connections' name ```mysql``` below:

    'connections' => [

        'sqlite' => [
            'driver' => 'sqlite',
            'database' =>  database_path('database.sqlite'),
            'prefix' => '',
        ],

        'mysql' => [
            'driver' => 'mysql',
            'host' => env('DB_HOST', 'localhost'),
            'port' => env('DB_PORT', '3306'),
            'database' => env('DB_DATABASE', 'forge'),
            'username' => env('DB_USERNAME', 'forge'),
            'password' => env('DB_PASSWORD', ''),
            'charset' => 'utf8',
            'collation' => 'utf8_unicode_ci',
            'prefix' => '',
            'strict' => false,
            'engine' => null,
        ],
    ],


If you did not mention the name of database connection in other codes or commands, Laravel will pick up the default database connection name. however, in multiple database connections, even you setup the default connection, you've better setup everywhere which database connection you used.


**Migration file**

In migration file, if single database connection, you can use:
  

     Schema::create("table",function(Blueprint $table){
         $table->increments('id');
    });

In multiple database connection, you will use the `connection()` method to tell Laravel which database connection you use:

     Schema::connection("sqlite")->create("table",function(Blueprint $table){
         $table->increments('id');
    });


**Artisan Migrate**

if you use single database connection, you will run:

    php artisan migrate

However, for multiple database connection, you've better tell which database connection maintains the migration data. so you will run the following command:

    php artisan migrate:install --database=sqlite

This command will install migration table in the target database to prepare migration.

    php artisan migrate --database=sqlite

This command will run migration and save the migration data in the target database

    php artisan migrate:rollback --database=sqlite

This command will rollback migration and save the migration data in the target database


**Eloquent Model**

To specify a database connection using Eloquent, you need to define the ```$connection``` property:

    namespace App\Model\Sqlite;
    class Table extends Model
    {
        protected $table="table";
        protected $connection = 'sqlite';
    }
To specify another ( second ) database connection using Eloquent:

    namespace App\Model\MySql;
    class Table extends Model
    {
        protected $table="table";
        protected $connection = 'mysql';
    }

Laravel will use ```$connection``` property defined in a model to utilize the specified connection defined in ```config/database.php```.  If the ```$connection``` property is not defined in a model the default will be used.

You may also specify another connection using the static ```on``` method:
    
    // Using the sqlite connection
    Table::on('sqlite')->select(...)->get()
    // Using the mysql connection
    Table::on('mysql')->select(...)->get()

**Database/Query Builder**

You may also specify another connection using the query builder:

    // Using the sqlite connection
    DB::connection('sqlite')->table('table')->select(...)->get()
    // Using the mysql connection
    DB::connection('mysql')->table('table')->select(...)->get()  

**Unit Test**

Laravel provide ```seeInDatabase($table,$fielsArray,$connection)``` to test database connection code. In Unit test file, you need to do like:

    $this
        ->json(
            'GET',
            'result1/2015-05-08/2015-08-08/a/123'
        )
         ->seeInDatabase("log", ["field"=>"value"], 'sqlite');


In this way, Laravel will know which database connection to test.                   

**Database Transactions in Unit Test**

Laravel allows database to rollback all the change during the tests. For testing multiple database connections, you need to set ```$connectionsToTransact ``` properties

    use Illuminate\Foundation\Testing\DatabaseMigrations;
    
    class ExampleTest extends TestCase
    {
         use DatabaseTransactions;
    
         $connectionsToTransact =["mysql","sqlite"] //tell Laravel which database need to rollBack
    
        public function testExampleIndex()
        {
            $this->visit('/action/parameter')
             ->see('items');
        }
    }



