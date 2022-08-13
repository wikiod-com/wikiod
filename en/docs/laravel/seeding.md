---
title: "Seeding"
slug: "seeding"
draft: false
images: []
weight: 9693
type: docs
toc: true
---

Database seeding allows you to insert data, general test data into your database. By default there is a `DatabaseSeeder` class under `database/seeds`.

Running seeders can be done with

    php artisan db:seed

Or if you only want to process a single class

    php artisan db:seed --class=TestSeederClass

As with all artisan commands, you have access to a wide array of methods which can be found in the [api documentation][1]


  [1]: https://laravel.com/api/5.3/Illuminate/Console/Command.html

## Creating a Seeder
To create seeders, you may use the `make:seeder` Artisan command. All seeders generated will be placed in the `database/seeds` directory.

```
$ php artisan make:seeder MoviesTableSeeder
```

Generated seeders will contain one method: `run`. You may insert data into your database in this method.

<!-- language: php -->
    <?php

    use Illuminate\Database\Seeder;
    use Illuminate\Database\Eloquent\Model;

    class MoviesTableSeeder extends Seeder
    {
        /**
         * Run the database seeds.
         *
         * @return void
         */
        public function run()
        {
            App\Movie::create([
                'name' => 'A New Hope',
                'year' => '1977'
            ]);

            App\Movie::create([
                'name' => 'The Empire Strikes Back',
                'year' => '1980'
            ]);
        }
    }

You will generally want to call all your seeders [inside the `DatabaseSeeder` class](https://www.wikiod.com/laravel/seeding#Calling other seeders).

Once you're done writing the seeders, use the `db:seed` command. This will run `DatabaseSeeder`'s `run` function.

```
$ php artisan db:seed
```

You may also specify to run a specific seeder class to run individually using the `--class` option.

```
$ php artisan db:seed --class=UserSeeder
```

If you want to rollback and rerun all migrations, and then reseed:

```
$ php artisan migrate:refresh --seed
```

> The `migrate:refresh --seed` command is a shortcut to these 3 commands:
> ```
> $ php artisan migrate:reset     # rollback all migrations
> $ php artisan migrate           # run migrations
> $ php artisan db:seed           # run seeders
> ```

## Inserting data
There are several ways to insert data:

## Using the DB Facade

<!-- language: php -->

    public function run()
    {
        DB::table('users')
            ->insert([
                'name' => 'Taylor',
                'age'  => 21
            ]);
    }

## Via Instantiating a Model

<!-- language: php -->

    public function run()
    {
        $user = new User;
        $user->name = 'Taylor';
        $user->save();
    }

## Using the create method

<!-- language: php -->

    public function run()
    {
        User::create([
            'name' => 'Taylor',
            'age'  => 21
        ]);
    }

## Using factory

<!-- language: php -->
    public function run()
    {
        factory(App\User::class, 10)->create();
    }

### Seeding && deleting old data and reseting auto-increment

<!-- language: php -->
    public function run()
    {
        DB::table('users')->delete();
        DB::unprepared('ALTER TABLE users AUTO_INCREMENT=1;');
        factory(App\User::class, 200)->create();
    }


See the [Persisting](https://www.wikiod.com/laravel/eloquent#Persisting) example for more information on inserting/updating data.

## Calling other seeders
Within your `DatabaseSeeder` class you are able to call other seeders

    $this->call(TestSeeder::class)

This allows you to keep one file where you can easily find your seeders.
Keep in mind that you need to pay attention to the order of your calls regarding foreign key constraints. You can't reference a table that doesn't exist yet.

## Safe reseeding
You may want to re-seed your database without affecting your previously created seeds. For this purpose, you can use [`firstOrCreate`][1] in your seeder:

<!-- language: php -->
    EmployeeType::firstOrCreate([
        'type' => 'manager',
    ]);

Then you can seed the database:

    php artisan db:seed

----------

Later, if you want to add another type of employee, you can just add that new one in the same file:

<!-- language: php -->
    EmployeeType::firstOrCreate([
        'type' => 'manager',
    ]);
    EmployeeType::firstOrCreate([
        'type' => 'secretary',
    ]);

And seed your database again with no problems:

    php artisan db:seed

Notice in the first call you are retrieving the record but doing nothing with it.

[1]: https://laravel.com/api/5.2/Illuminate/Database/Eloquent/Builder.html#method_firstOrCreate

