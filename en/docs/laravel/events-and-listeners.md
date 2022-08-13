---
title: "Events and Listeners"
slug: "events-and-listeners"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Using Event and Listeners for sending emails to a new registered user
Laravel's events allows to implement the Observer pattern. This can be used to send a welcome email to a user whenever they register on your application.

New events and listeners can be generated using the artisan command line utility after registering the event and their particular listener in `App\Providers\EventServiceProvider` class.

<!-- language-all: php -->

    protected $listen = [
        'App\Events\NewUserRegistered' => [
            'App\Listeners\SendWelcomeEmail',
        ],
    ];
 Alternate notation:

    protected $listen = [
        \App\Events\NewUserRegistered::class => [
            \App\Listeners\SendWelcomeEmail::class,
        ],
    ];

Now execute `php artisan generate:event`. This command will generate all the corresponding events and listeners mentioned above in `App\Events` and `App\Listeners` directories respectively.

We can have multiple listeners to a single event like 

    protected $listen = [
        'Event' => [
            'Listner1', 'Listener2'
        ],
    ];

`NewUserRegistered` is just a wrapper class for the newly registered User model:

    class NewUserRegistered extends Event
    {
        use SerializesModels;
    
        public $user;
    
        /**
         * Create a new event instance.
         *
         * @return void
         */
        public function __construct(User $user)
        {
            $this->user = $user;
        }
    }

This `Event` will be handled by the `SendWelcomeEmail` listener:

    class SendWelcomeEmail
    {
        /**
         * Handle the event.
         *
         * @param  NewUserRegistered  $event
         */
        public function handle(NewUserRegistered $event)
        {
            //send the welcome email to the user
            $user = $event->user;
            Mail::send('emails.welcome', ['user' => $user], function ($message) use ($user) {
                    $message->from('hi@yourdomain.com', 'John Doe');
                    $message->subject('Welcome aboard '.$user->name.'!');
                    $message->to($user->email);
            });
        }
    }

The last step is to call/fire the event whenever a new user registers. This can be done in the controller, command or service, wherever you implement the user registration logic:


    event(new NewUserRegistered($user));

