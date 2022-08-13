---
title: "Meteor User Accounts"
slug: "meteor-user-accounts"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Meteor accounts package
You have a few options when it comes to logging in with Meteor. The most common method is using `accounts` for Meteor.


Accounts-password
===============
If you want users to be able to create and register on your site, you can use `accounts-password`.

Install the package using `meteor add accounts-password`.

To create a user, you need to use `Accounts.createUser(options, [callback])`

`options` has to be an object with the following properties:

 - `username`: The user's username as a string..
 - `email`: The user's email as a string.
 - `password`: The user's (not encrypted) password as a string.
 - `profile`: The user's optional extra data as an object. This can be for example the user's first and last name. `profile` is optional, however.

The callback returns 1 variable if there is an error, which is a Meteor.Error object.

You are only required to use either the `username` or the `email`, so you can create a user with username but no email, and vice versa. You can also use both.

It returns the newly created user ID if everything went correctly.

So, you can for example use this:

    // server side
    var id = Accounts.createUser({
        username: "JohnDoe",
        email: "JohnDoe@gmail.com",
        password: "TheRealJohn123",
        profile: {
            firstName: "John",
            lastName: "Doe"
        }
    }, function(err) {
        console.log(err.reason);
    });

It will automatically log you in as well if the user was succesfully created.

That is the creating part.
To log in you need to use `Meteor.loginWithPassword(identifier, password, [callback])` on the client side.

`identifier` is the `username`, `email` or `userId` as a string from your user.
`password` is the (not encrypted) `password` of the user.

The callback returns one variable if there is an error, which is a Meteor.Error object.

Example:

    // client side
    Meteor.loginWithPassword("JohnDoe", "TheRealJohn123", function(err) {
        console.log(err.reason);
    });

And that is it for the basic creating of accounts and logging in.

Accessing user data
========================
You can check on the client side if the user is logged in by calling `Meteor.userId()` which will return their `userId` if they are logged in, and `undefined` if they are not logged in.

You can get some of the info from `Meteor.user()`. It will return undefined if the user is not logged in, and some user data if they are. It will not give you any passwords by default, by default it will show the userId of the user, the username and the profile object.

If you want to check if a user is logged in on a page, you can also use the `currentUser` helper. It will return the contents of `Meteor.user()`. Example:

    {{#if currentUser}}
        <h1>Hello there, {{currentUser.username}}!</h1>
    {{else}}
        <h1>Please log in.</h1>
    {{/if}}

Other accounts functions
===================================
There are some other functions that work for every accounts package.

You can log out using `Meteor.logout()`

## Don’t use the default profile field
There’s a tempting existing field called `profile` that is added by default when a new user registers. This field was historically intended to be used as a scratch pad for user-specific data - maybe their image avatar, name, intro text, etc. Because of this, **the `profile` field on every user is automatically writable by that user from the client**. It’s also automatically published to the client for that particular user.

It turns out that having a field writable by default without making that super obvious might not be the best idea. There are many stories of new Meteor developers storing fields such as `isAdmin` on `profile`… and then a malicious user can easily set that to true whenever they want, making themselves an admin. Even if you aren’t concerned about this, it isn’t a good idea to let malicious users store arbitrary amounts of data in your database.

Rather than dealing with the specifics of this field, it can be helpful to just ignore its existence entirely. You can safely do that as long as you deny all writes from the client:

    // Deny all client-side updates to user documents
    Meteor.users.deny({
      update() { return true; }
    });

Even ignoring the security implications of profile, it isn’t a good idea to put all of your app’s custom data onto one field. Meteor’s data transfer protocol doesn’t do deeply nested diffing of fields, so it’s a good idea to flatten out your objects into many top-level fields on the document.

