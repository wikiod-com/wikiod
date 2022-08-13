---
title: "Task Scheduling"
slug: "task-scheduling"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Creating a task
You can create a task (Console Command) in Laravel using Artisan. From your command line:

    php artisan make:console MyTaskName

This creates the file in **app/Console/Commands/MyTaskName.php**. It will look like this:

    <?php

    namespace App\Console\Commands;

    use Illuminate\Console\Command;

    class MyTaskName extends Command
    {
        /**
         * The name and signature of the console command.
         *
         * @var string
         */
        protected $signature = 'command:name';

        /**
         * The console command description.
         *
         * @var string
         */
        protected $description = 'Command description';

        /**
         * Create a new command instance.
         *
         * @return void
         */
        public function __construct()
        {
            parent::__construct();
        }

        /**
         * Execute the console command.
         *
         * @return mixed
         */
        public function handle()
        {
            //
        }
    }

Some important parts of this definition are:

- The `$signature` property is what identifies your command. You will be able to execute this command later through the command line using Artisan by running `php artisan command:name` (Where `command:name` matches your command's `$signature`)
- The `$description` property is Artisan's help/usage displays next to your command when it is made available.
- The `handle()` method is where you write the code for your command.

Eventually, your task will be made available to the command line through Artisan. The `protected $signature = 'command:name';` property on this class is what you would use to run it.




## Making a task available
You can make a task available to Artisan and to your application in the **app/Console/Kernel.php** file.

The `Kernel` class contains an array named `$commands` which make your commands available to your application.

Add your command to this array, in order to make it available to Artisan and your application.

    <?php

    namespace App\Console;

    use Illuminate\Console\Scheduling\Schedule;
    use Illuminate\Foundation\Console\Kernel as ConsoleKernel;

    class Kernel extends ConsoleKernel
    {
        /**
         * The Artisan commands provided by your application.
         *
         * @var array
         */
        protected $commands = [
            Commands\Inspire::class,
            Commands\MyTaskName::class // This line makes MyTaskName available
        ];

        /**
         * Define the application's command schedule.
         *
         * @param  \Illuminate\Console\Scheduling\Schedule  $schedule
         * @return void
         */
        protected function schedule(Schedule $schedule)
        {
        
        }
    }

Once this is done, you can now access your command via the command line, using Artisan. Assuming that your command has the `$signature` property set to `my:task`, you can run the following command to execute your task:

    php artisan my:task




## Scheduling your task
When your command is made available to your application, you can use Laravel to schedule it to run at pre-defined intervals, just like you would a CRON.

In The **app/Console/Kernel.php** file you will find a `schedule` method that you can use to schedule your task.

    <?php

    namespace App\Console;

    use Illuminate\Console\Scheduling\Schedule;
    use Illuminate\Foundation\Console\Kernel as ConsoleKernel;

    class Kernel extends ConsoleKernel
    {
        /**
         * The Artisan commands provided by your application.
         *
         * @var array
         */
        protected $commands = [
            Commands\Inspire::class,
            Commands\MyTaskName::class
        ];

        /**
         * Define the application's command schedule.
         *
         * @param  \Illuminate\Console\Scheduling\Schedule  $schedule
         * @return void
         */
        protected function schedule(Schedule $schedule)
        {
            $schedule->command('my:task')->everyMinute();
            // $schedule->command('my:task')->everyFiveMinutes();
            // $schedule->command('my:task')->daily();
            // $schedule->command('my:task')->monthly();
            // $schedule->command('my:task')->sundays();
        }
    }

Assuming your task's `$signature` is `my:task` you can schedule it as shown above, using the `Schedule $schedule` object. Laravel provides loads of different ways to schedule your command, as shown in the commented out lines above.



## Setting the scheduler to run
The scheduler can be run using the command:

    php artisan schedule:run

The scheduler needs to be run every minute in order to work correctly. You can set this up by creating a cron job with the following line, which runs the scheduler every minute in the background.

    * * * * * php /path/to/artisan schedule:run >> /dev/null 2>&1

