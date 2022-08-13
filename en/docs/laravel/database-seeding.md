---
title: "Database Seeding"
slug: "database-seeding"
draft: false
images: []
weight: 9890
type: docs
toc: true
---

## Running a Seeder
You may add your new Seeder to the DatabaseSeeder class.

        /**
         * Run the database seeds.
         *
         * @return void
         */
        public function run()
        {
            $this->call(UserTableSeeder::class);
        }

To run a database seeder, use the Artisan command

    php artisan db:seed

This will run the DatabaseSeeder class. You can also choose to use the `--class=` option to manually specify which seeder to run.

*Note, you may need to run composer dumpautoload if your Seeder class cannot be found. This typically happens if you manually create a seeder class instead of using the artisan command.

## Creating a Seed
Database seeds are stored in the /database/seeds directory. You can create a seed using an Artisan command.

    php artisan make:seed UserTableSeeder

Alternatively you can create a new class which extends `Illuminate\Database\Seeder`. The class must a public function named `run()`.


## Inserting Data using a Seeder
You can reference models in a seeder.

    use DB;
    use App\Models\User;
    
    class UserTableSeeder extends Illuminate\Database\Seeder{

        public function run(){
            # Remove all existing entrie
            DB::table('users')->delete() ;
            User::create([
                'name' => 'Admin',
                'email' => 'admin@example.com',
                'password' => Hash::make('password')
            ]);

        }
    }

## Inserting data with a Model Factory
You may wish to use Model Factories within your seeds. This will create 3 new users.

    use App\Models\User;
    
    class UserTableSeeder extends Illuminate\Database\Seeder{
    
        public function run(){
            factory(User::class)->times(3)->create();    
        }
    }

You may also want to define specific fields on your seeding like a password, for instance. This will create 3 users with the same password.

    factory(User::class)->times(3)->create(['password' => '123456']);    



## Seeding with MySQL Dump
Follow previous example of creating a seed. This example uses a MySQL Dump to seed a table in the project database. The table must be created before seeding.


    <?php
    
    use Illuminate\Database\Seeder;
    
    class UserTableSeeder extends Seeder
    {
    
        /**
         * Run the database seeds.
         *
         * @return void
         */
         public function run()
         {
             $sql = file_get_contents(database_path() . '/seeds/users.sql');
        
             DB::statement($sql);
          }
    }

Our $sql is going to be the contents of our users.sql dump. The dump should have an INSERT INTO statement. It will be up to you where you store your dumps. In the above example, it is stored in the project directory `\database\seeds`. Using laravel's helper function `database_path()` and appending the directory and file name of the dump.

    INSERT INTO `users` (`id`, `name`, `email`, `password`, `remember_token`, `created_at`, `updated_at`) VALUES
    (1, 'Jane', 'janeDoe@fakemail.com', 'superSecret', NULL, '2016-07-21 00:00:00', '2016-07-21 00:00:00'),
    (2, 'John', 'johnny@fakemail.com', 'sup3rS3cr3t', NULL, '2016-07-21 00:00:00', '2016-07-21 00:00:00');

`DB::statement($sql)` will execute the inserts once the Seeder is run. As in previous examples, you can put the `UserTableSeeder` in the `DatabaseSeeder` class provided by laravel:


    <?php
    
    use Illuminate\Database\Seeder;
    
    class DatabaseSeeder extends Seeder
    {
        /**
         * Run the database seeds.
         *
         * @return void
         */
        public function run()
        {
            $this->call(UserTableSeeder::class);
        }
    }

and run from CLI in project directory `php artisan db:seed`. Or you can run the Seeder for a single class using `php artisan db:seed --class=UsersTableSeeder`

## Using faker And ModelFactories to generate Seeds


**1) BASIC SIMPLE WAY**


Database-driven applications often need data pre-seeded into the system for testing and demo purposes.


To make such data, first create the seeder class 

**ProductTableSeeder** 

    use Faker\Factory as Faker;
    use App\Product;
    
    class ProductTableSeeder extends DatabaseSeeder {
    
    public function run()
    {
        $faker = $this->getFaker();
    
        for ($i = 0; $i < 10; $i++)
        {
            $name =         $faker->word;
            $image =        $faker->imageUrl;
           
            Modelname::create([
                'name' => $name,
                'image' => $image,
            ]);
          }
        }
     }

To call a be able to execute a seeder class, you have call it from the DatabaseSeeder class, Simply by passing the name of the seeder you wish to run:

use Illuminate\Database\Seeder;

    class DatabaseSeeder extends Seeder {
    
        protected $faker;
    
        public function getFaker() {
            if (empty($this->faker)) {
                $faker = Faker\Factory::create();
                $faker->addProvider(new Faker\Provider\Base($faker));
                $faker->addProvider(new Faker\Provider\Lorem($faker));
            }
            return $this->faker = $faker;
        }
        public function run() {
            $this->call(ProductTableSeeder::class);
        }
    }



Do not forget to run `$ composer dump-autoload` after you create the Seeder, since they are not automatically autoloaded by composer (unless you created seeder by artisan command `$ php artisan make:seeder Name`)

Now you are ready to seed by running this artisan command `php artisan db:seed`



**2) USING Model Factories**


First of all you to define a default set of attributes for each Model in `App/database/factories/ModelFactory.php` 

Taking a User model as an exemple, This how a ModelFactory looks like


    $factory->define(App\User::class, function (Faker\Generator $faker) {
        return [
            'name' => $faker->name,
            'email' => $faker->email,
            'password' => bcrypt(str_random(10)),
            'remember_token' => str_random(10),
        ];
    });


Now Create a table seeder `php artisan make:seeder UsersTableSeeder`

And add this

    public function run()
    {
        factory(App\User::class, 100)->create()
    }

then add this to the `DatabaseSeeder`

    public function run()
    {
        $this->call(UsersTableSeeder::class);
    }

This will seed the table with 100 records.







