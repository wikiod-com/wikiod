---
title: "Artisan"
slug: "artisan"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

## Syntax
 - php artisan [command] [options] [arguments]

## Parameters
| Command | Description |
| ------ | ------ |
| clear-compiled     | Remove the compiled class file |
| down               | Put the application into maintenance mode |
| env                | Display the current framework environment |
| help               | Displays help for a command |
| list               | Lists commands |
| migrate            | Run the database migrations |
| optimize           | Optimize the framework for better performance |
| serve              | Serve the application on the PHP development server |
| tinker             | Interact with your application |
| up                 | Bring the application out of maintenance mode |
| app:name           | Set the application namespace |
| auth:clear-resets  | Flush expired password reset tokens |
| cache:clear        | Flush the application cache |
| cache:table        | Create a migration for the cache database table |
| config:cache       | Create a cache file for faster configuration loading |
| config:clear       | Remove the configuration cache file |
| db:seed            | Seed the database with records |
| event:generate     | Generate the missing events and listeners based on registration |
| key:generate       | Set the application key |
| make:auth          | Scaffold basic login and registration views and routes |
| make:console       | Create a new Artisan command |
| make:controller    | Create a new controller class |
| make:event         | Create a new event class |
| make:job           | Create a new job class |
| make:listener      | Create a new event listener class |
| make:middleware    | Create a new middleware class |
| make:migration     | Create a new migration file |
| make:model         | Create a new Eloquent model class |
| make:policy        | Create a new policy class |
| make:provider      | Create a new service provider class |
| make:request       | Create a new form request class |
| make:seeder        | Create a new seeder class |
| make:test          | Create a new test class |
| migrate:install    | Create the migration repository |
| migrate:refresh    | Reset and re-run all migrations |
| migrate:reset      | Rollback all database migrations |
| migrate:rollback   | Rollback the last database migration |
| migrate:status     | Show the status of each migration |
| queue:failed       | List all of the failed queue jobs |
| queue:failed-table | Create a migration for the failed queue jobs database table |
| queue:flush        | Flush all of the failed queue jobs |
| queue:forget       | Delete a failed queue job |
| queue:listen       | Listen to a given queue |
| queue:restart      | Restart queue worker daemons after their current job |
| queue:retry        | Retry a failed queue job |
| queue:table        | Create a migration for the queue jobs database table |
| queue:work         | Process the next job on a queue |
| route:cache        | Create a route cache file for faster route registration |
| route:clear        | Remove the route cache file |
| route:list         | List all registered routes |
| schedule:run       | Run the scheduled commands |
| session:table      | Create a migration for the session database table |
| vendor:publish     | Publish any publishable assets from vendor packages |
| view:clear         | Clear all compiled view files |

## Introduction
Artisan is a utility that can help you do specific repetitive tasks with bash commands. It covers many tasks, including: working with database [**migrations**][1] and [**seeding**][2], clearing **cache**, creating necessary files for **Authentication** setup, **making** new *controllers, models, event classes*, and a lot more.

> Artisan is the name of the command-line interface included with Laravel. It provides a number of helpful commands for your use while developing your application.

To view a list of all available Artisan commands, you may use the list command:

    php artisan list

To know more about the any available command, just precede its name with **help** keyword:

    php artisan help [command-name]

 [1]: https://www.wikiod.com/laravel/database-migrations#Migrations
 [2]: https://www.wikiod.com/laravel/seeding

## List all registered routes filtered by multiple methods
<!-- language: lang-sh -->

```
php artisan route:list --method=GET --method=POST
```

This will include all routes that accept `GET` and `POST` methods simultaneously.

## Creating and registering new artisan command
You can create new commands via 

`php artisan make:command [commandName]`

So this will create [commandName] command class inside `app/Console/Commands` directory.

inside this class you will find `protected $signature` and `protected $description` variables, it represents name and discription of your command 
which will be used to describe your command.

after creating command you can register your command inside `app/Console/Kernel.php` class where you will find `commands` property.

so you can add your command inside the $command array like :

    protected $commands = [
        Commands\[commandName]::class
    ];

and then i can use my command via console.

so as example i have named my command like

    protected $signature = 'test:command';

So whenever i will run 

    php artisan test:command

it will call the `handle` method inside the class having signature `test:command`.

## Running Laravel Artisan commands using PHP code
You can also use Laravel Artisan commands from your routes or controllers.

To run a command using PHP code:

    Artisan::call('command-name');

For example,

    Artisan::call('db:seed');




