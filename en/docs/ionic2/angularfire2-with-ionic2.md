---
title: "Angularfire2 with Ionic2"
slug: "angularfire2-with-ionic2"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Here ill show you how to integrate AngularFire2 and use this real time database in our Ionic App.

## AngularFire initialization
First of all you need to initialize the angularfire modules in your app module like this:

      const firebaseConfig = {
      apiKey: 'XXXXXXXXXX',
      authDomain: 'XXXXXXXXXX',
      databaseURL: 'XXXXXXXXXX',
      storageBucket: 'XXXXXXXXXX',
      messagingSenderId: 'XXXXXXXXXX'
    };

You can get this keys by signing on firebase and creating a new project.

    imports: [
        AngularFireModule.initializeApp(firebaseConfig),
        AngularFireDatabaseModule,
        AngularFireAuthModule
      ],

## Using AngularFire2
Once you have it on your app, just import it:

    import { AngularFireDatabase } from 'angularfire2/database';
    constructor (private _af: AngularFireDatabase) {}

With this Observable List you can access to a list of items under a path, for example if you have root/items/food you can get food items like this:

    this._af.list('root/items/food');

And you can simple put a new item here and will appear on your firebase database, or you can update one item and you will see it update on your database. You can push and update like this:

    this._af.list('root/items/food').push(myItemData);
    this._af.list('root/items/food').update(myItem.$key, myNewItemData);

Or you can even remote items from your food list:

    this._af.list('root/items/food').remove(myItem.$key);


