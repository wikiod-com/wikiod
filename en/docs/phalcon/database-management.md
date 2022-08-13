---
title: "Database Management"
slug: "database-management"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Database management using Phalcon Model
A model for a new table can be created by running the following commend from the terminal root location:

    phalcon model <table-name>

Let us take the Model Users.


**SELECT**

There are two default functions to do select operation in phalcon, `find()` and `findFirst()`

`findFirst()` is used to get the first row which satisfies the conditions that we are passing. It returns a single object with the data in first row.

Example:

    $user = Users::findFirst("active=1 AND verified=1 AND email='a@a.com'");

This returns the user with the given email and the value of the column verified and active is 1


`find()` is used to get all rows which satisfies the conditions we are passing.

Example:

    $users = Users::find("active=1 AND verified=1");

This returns the users with the value of the column verified and active is 1
            


**INSERT**

Insert can be done using the following code:

    $user = new Users();
    
    $user->name = "Arun";
    $user->email = "abc@gmail.com";
    $user->verified = 1;
    $user->active = 1;
    
    $user->save();

A new row with the these values will be inserted.





**UPDATE**

Update can be done using the following code:

First we have to select the row we have to update using `findFirst()`

    $user = Users::findFirst("email='a@a.com'");
    
    $user->verified = 0;
    $user->active = 0;
    
    $user->save();

This will change the values for the column verified and active for the row with given email.


**DELETE**
Delete can also be done using the findFirst()

Example:

    Users::findFirst("email='a@a.com'")->delete();

This will delete the row with given email.



You can also execute custom sql commands with models using the following code:

    $query = $this->modelsManager->createQuery("SELECT * FROM Users WHERE email='a@a.com'");
    
    $user = $query->execute();


## Using standard SQL directly with models
To use SQL syntax with model, that would transfer result to proper instantions, you should use directly one of Phalcon\Mvc\Model\Resultset classes:

    $users = new \Application\Models\Users();
    
    // bitwise operation on `flag` field
    $sql = 'SELECT * FROM phorum.users WHERE
            (15 & (1 << (flag - 1))) > 0  ORDER BY login DESC';
    
    // as a result you will have a Resultset\Simple with Models\Users instances.
    $result = new \Phalcon\Mvc\Model\Resultset\Simple(
        null,

        // what model to use for data returned from SQL
        $users,

        // setting result via "read connection" proper for this model.
        $users->getReadConnection()->query($sql) 
    );



## Setting up default connection service
Phalcon uses `db` service by default to obtain connection to databases.

Assuming you have an conguration file with `database` field set up, you can include or autoload following code to obtain connection with database for your project:

    $di->set('db', function () use ($config) {
        $dbconf = $config->database;
        switch(strtolower($dbconf->adapter)) {

            case 'mysql':
                return new \Phalcon\Db\Adapter\Pdo\Mysql(array(
                    'host' => $dbconf->host,
                    'username' => $dbconf->username,
                    'password' => $dbconf->password,
                    // default database to work with
                    'dbname' => $dbconf->dbname,
                    // default character set
                    'charset' => $dbconf->charset,
                    // connection warm-up commands for PDO
                    'options' => array(
                        PDO::MYSQL_ATTR_INIT_COMMAND => 'SET NAMES "' . $dbconf->charset . '"',
                        PDO::ATTR_CASE => PDO::CASE_LOWER
                    )
                ));

            case 'postgresql':
                return new \Phalcon\Db\Adapter\Pdo\Postgresql(array(
                    'host' => $dbconf->host,
                    'username' => $dbconf->username,
                    'password' => $dbconf->password,
                    'dbname' => $dbconf->dbname,
                    'options' => array(
                    )
                ));
            
            default: 
                throw new \Exception('Unimplemented database::adapter in config.ini');
        }
    });

## Caching Models Meta-Data.
Phalcon builds up some information about tables it is using, so it is possible to validate data being inserted to them without implementing everything by hand. Those are meta data for models. To speed up and prevent Phalcon from building Meta Data every time page is refreshed, it is possible to cache them. To do so, you need to implement `metaData` service for it to use:

    $di->set('modelsMetadata', function() use ($config)
    {
        // assuming that you have a $config var with
        // models.metadata.adapter field declared
        switch (strtolower($config->models->metadata->adapter)) {
            case 'apc':
                $metaData = new MetaDataApcAdapter([
                    'lifetime' => $config->models->metadata->lifetime,
                    'suffix' => $config->models->metadata->suffix,
                ]);
                break;
            case 'xcache':
                $metaData = new MetaDataXCacheAdapter([
                    'lifetime' => $config->models->metadata->lifetime,
                    'prefix' => $config->models->metadata->suffix,
                ]);
                break;
            case 'memory':
                $metaData = new MetaDataMemoryAdapter();
                break;
            default:
                throw new \Exception('Unimplemented models::metadata.adapter in config.ini');
        }

        return $metaData;
    });

Further documentation available at Phalcons' [dedicated page](https://docs.phalconphp.com/pl/latest/reference/models-metadata.html).

