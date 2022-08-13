---
title: "Cashier"
slug: "cashier"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Laravel Cashier can be used for subscription billing by providing an interface into the subscription services of both Braintree and Stripe. In addition to basic subscription management it can be used to handle coupons, exchanging subscriptions, quantities, cancellation grace periods and PDF invoice generation.

## Stripe Setup
**Initial Setup**

To use Stripe for handling payments we need to add the following to the `composer.json` then run `composer update`:

<!-- language: lang-php -->
```
"laravel/cashier": "~6.0"
``` 
The following line then needs to be added to `config/app.php`, the service provider:
<!-- language: lang-php -->
```
Laravel\Cashier\CashierServiceProvider
```

----
**Databse Setup**

In order to use cashier we need to configure the databases, if a users table does not already exist we need to create one and we also need to create a subscriptions table. The following example amends an existing `users` table.
See [Eloquent Models][1] for more information about models.

To use cashier create a new migration and add the following which will achieve the above:

<!-- language: lang-php -->
```
// Adjust users table

Schema::table('users', function ($table) {
    $table->string('stripe_id')->nullable();
    $table->string('card_brand')->nullable();
    $table->string('card_last_four')->nullable();
    $table->timestamp('trial_ends_at')->nullable();
});

//Create subscriptions table

Schema::create('subscriptions', function ($table) {
    $table->increments('id');
    $table->integer('user_id');
    $table->string('name');
    $table->string('stripe_id');
    $table->string('stripe_plan');
    $table->integer('quantity');
    $table->timestamp('trial_ends_at')->nullable();
    $table->timestamp('ends_at')->nullable();
    $table->timestamps();
});
```
We then need to run `php artisan migrate` to update our database.

---
**Model Setup**

We then have to add the billable trait to the User model found in `app/User.php` and change it to the following:

<!-- language: lang-php -->
```
use Laravel\Cashier\Billable;

class User extends Authenticatable
{
    use Billable;
}
```

---

**Stripe Keys**

In order to ensure that we ares ending the money to our own Stripe account we have to set it up in the `config/services.php` file by adding the following line:

<!-- language: lang-php -->
```
'stripe' => [
    'model'  => App\User::class,
    'secret' => env('STRIPE_SECRET'),
],
```
Replacing the `STRIPE_SECRET` with your own stripe secret key.

---

After completing this Cashier and Strip is setup so you can continue with setting up subscriptions.

  [1]: https://www.wikiod.com/laravel/eloquent

