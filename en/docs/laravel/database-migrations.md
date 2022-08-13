---
title: "Database Migrations"
slug: "database-migrations"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Inside a database migration
Each migration should have an `up()` method and a `down()` method. The purpose of the `up()` method is to perform the required operations to put the database schema in its new state, and the purpose of the `down()` method is to reverse any operations performed by the `up()` method. Ensuring that the `down()` method correctly reverses your operations is critical to being able to rollback database schema changes.

An example migration file may look like this:

<!-- language: lang-php -->
    <?php
    
    use Illuminate\Database\Schema\Blueprint;
    use Illuminate\Database\Migrations\Migration;
    
    class AddLastLoggedInToUsersTable extends Migration
    {
        /**
         * Run the migrations.
         *
         * @return void
         */
        public function up()
        {
            Schema::table('users', function (Blueprint $table) {
                $table->dateTime('last_logged_in')->nullable();
            });
        }
    
        /**
         * Reverse the migrations.
         *
         * @return void
         */
        public function down()
        {
            Schema::table('users', function (Blueprint $table) {
                $table->dropColumn('last_logged_in');
            });
        }
    }

When running this migration, Laravel will generate the following SQL to run against your database:

<!-- language: lang-sql -->
    ALTER TABLE `users` ADD `last_logged_in` DATETIME NULL

## Migrations
To control your database in Laravel is by using migrations. Create migration with artisan:

<!-- language: lang-bash -->

    php artisan make:migration create_first_table --create=first_table

This will generate the class CreateFirstTable. Inside the up method you can create your columns:

<!-- language: lang-php -->

    <?php
    
    use Illuminate\Database\Schema\Blueprint;
    use Illuminate\Database\Migrations\Migration;
    
    class CreateFirstTable extends Migration
    {
        public function up()
        {
            Schema::create('first_table', function (Blueprint $table) {
                $table->increments('id');
                $table->string('first_string_column_name');
                $table->integer('secont_integer_column_name');
                $table->timestamps();
            });
        }
    
        public function down()
        {
            Schema::drop('first_table');
        }
    } 

At the end to run all of your migrations classes you can run the artisan command:

<!-- language: lang-bash -->

    php artisan migrate

This will create your tables and your columns in your database. Other useful migrate command are:
-  `php artisan migrate:rollback` - Rollback the last database migration
- `php artisan migrate:reset`     -  Rollback all database migrations
- ` php artisan migrate:refresh`  -   Reset and re-run all migrations
- `php artisan migrate:status`   -   Show the status of each migration

**Modifying existing tables**

Sometimes, you need to change your existing table structure like `renaming/deleting` columns. Which you can accomplish by creating a new migration.And In the `up` method of your migration.

    //Renaming Column.
    
    public function up()
    {
        Schema::table('users', function (Blueprint $table) {
            $table->renameColumn('email', 'username');
        });
    }

 Above example will rename `email column` of `users table` to `username`. While the below code drops a column `username` from `users` table. 

IMPROTANT : For modifying columns you need to add `doctrine/dbal` dependency to project's `composer.json` file and run `composer update` to reflect changes.

    //Droping Column
    public function up()
    {
        Schema::table('users', function (Blueprint $table) {
            $table->dropColumn('username');
        });
    }

## Generating migration files
Creating a new migration file with the correct filename every time you need to change your schema would be a chore. Thankfully, Laravel's `artisan` command can generate the migration for you:

    php artisan make:migration add_last_logged_in_to_users_table

You can also use the `--table` and `--create` flags with the above command. These are optional and just for convenience, and will insert the relevant boilerplate code into the migration file.

    php artisan make:migration add_last_logged_in_to_users_table --table=users

    php artisan make:migration create_logs_table --create=logs

You can specify a custom output path for the generated migration using the `--path` option. The path is relative to the application's base path.

    php artisan make:migration --path=app/Modules/User/Migrations

## The migration files
Migrations in a Laravel 5 application live in the `database/migrations` directory. Their filenames conform to a particular format:

    <year>_<month>_<day>_<hour><minute><second>_<name>.php

One migration file should represent a schema update to solve a particular problem. For example:

    2016_07_21_134310_add_last_logged_in_to_users_table.php

Database migrations are kept in chronological order so that Laravel knows in which order to execute them. Laravel will always execute migrations from oldest to newest.

## Running migrations
Once your migration is written, running it will apply the operations to your database. 

    php artisan migrate

If all went well, you'll see an output similar to the below:

    Migrated: 2016_07_21_134310_add_last_logged_in_to_users_table

Laravel is clever enough to know when you're running migrations in the production environment. If it detects that you're performing a destructive migration (for example, one that removes a column from a table), the `php artisan migrate` command will ask you for confirmation. In continuous delivery environments this may not be wanted. In that case, use the `--force` flag to skip the confirmation:

    php artisan migrate --force

If you've only just run migrations, you may be confused to see the presence of a `migrations` table in your database. This table is what Laravel uses to keep track of what migrations have already been run. When issuing the `migrate` command, Laravel will determine what migrations have yet to run, and then execute them in chronological order, and then update the `migrations` table to suit.

You should never manually edit the `migrations` table unless you absolutely know what you're doing. It's very easy to inadvertently leave your database in a broken state where your migrations will fail.

## Rolling Back Migrations
What if you want to rollback the latest migration i.e recent operation, you can use the awesome `rollback` command. But remember that this command rolls back only the last migration, which may include multiple migration files

<!-- language: lang-bash -->

    php artisan migrate:rollback

If you are interested in rolling back all of your application migrations, you may use the following command

<!-- language: lang-bash -->

    php artisan migrate:reset 

Moreover if you are lazy like me and want to rollback and migrate with one command, you may use this command

<!-- language: lang-bash -->

    php artisan migrate:refresh
    php artisan migrate:refresh --seed

You can also specify number of steps to rollback with `step` option. Like this will rollback 1 step.

    php artisan migrate:rollback --step=1


 

