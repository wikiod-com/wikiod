---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

## Introduction
Writing testable code is an important part of building a robust, maintainable, and agile project. Support for PHP's most widely used testing framework, [PHPUnit](https://phpunit.de/), is built right into Laravel. PHPUnit is configured using the `phpunit.xml` file, which resides in the root directory of every new Laravel application.

The `tests` directory, also in the root directory, contains the individual testing files which hold the logic for testing each portion of your application. Of course, it is your responsibility as a developer to write these tests as you build your application, but Laravel includes an example file, `ExampleTest.php`, to get you going.

```
<?php

use Illuminate\Foundation\Testing\WithoutMiddleware;
use Illuminate\Foundation\Testing\DatabaseMigrations;
use Illuminate\Foundation\Testing\DatabaseTransactions;

class ExampleTest extends TestCase
{
    /**
     * A basic functional test example.
     *
     * @return void
     */
    public function testBasicExample()
    {
        $this->visit('/')
             ->see('Laravel 5');
    }
}
```

In the `testBasicExample()` method, we visit the site's index page and make sure we see the text `Laravel 5` somewhere on that page. If the text is not present, the test will fail and generate an error.

## Configuration
<!-- language-all: xml -->

The [phpunit.xml][1] file is the default configuration file for tests and is already setup for testing with PHPUnit.

The default testing environment `APP_ENV` is defined as `testing` with `array` being the cache driver `CACHE_DRIVER`. With this setup, no data (session/cache) will be retained while testing.

To run tests against a specific environment like homestead the defaults can be changed to:

    <env name="DB_HOST" value="192.168.10.10"/>
    <env name="DB_DATABASE" value="homestead"/>
    <env name="DB_USERNAME" value="homestead"/>
    <env name="DB_PASSWORD" value="secret"/>

Or to use a temporary *in memory* database:

    <env name="DB_CONNECTION" value="sqlite"/>
    <env name="DB_DATABASE" value=":memory:"/>

One last note to keep in mind from the [Laravel documentation][2]:

> Make sure to clear your configuration cache using the `config:clear`
> Artisan command before running your tests!


  [1]: https://github.com/laravel/laravel/blob/master/phpunit.xml
  [2]: https://laravel.com/docs/master/testing

## Test without middleware and with a fresh database
To make artisan migrate a fresh database before running tests, `use DatabaseMigrations`. Also if you want to avoid middleware like Auth, `use WithoutMiddleware`. 

    <?php
    
    use Illuminate\Foundation\Testing\WithoutMiddleware;
    use Illuminate\Foundation\Testing\DatabaseMigrations;
    
    class ExampleTest extends TestCase
    {
        use DatabaseMigrations, WithoutMiddleware;
    
        /**
         * A basic functional test example.
         *
         * @return void
         */
        public function testExampleIndex()
        {
            $this->visit('/protected-page')
             ->see('All good');
        }
    }

## Database transactions for mutliple database connection
<!-- language-all: lang-php --> 
```DatabaseTransactions``` trait allows databases to rollback all the change during the tests. If you want to rollback multiple databases , you need to set ```$connectionsToTransact ``` properties

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


## Testing setup, using in memory database
Following setup ensures that testing framework (PHPUnit) uses `:memory:` database.

**config/database.php**

    'connections' => [

        'sqlite_testing' => [
            'driver'   => 'sqlite',
            'database' => ':memory:',
            'prefix'   => '',
        ],
        .
        .
        .

**./phpunit.xml**
        
        .
        .
        .
        </filter>
        <php>
            <env name="APP_ENV" value="testing"/>
            <env name="APP_URL" value="http://example.dev"/>
            <env name="CACHE_DRIVER" value="array"/>
            <env name="SESSION_DRIVER" value="array"/>
            <env name="QUEUE_DRIVER" value="sync"/>
            <env name="DB_CONNECTION" value="sqlite_testing"/>
        </php>
    </phpunit>

