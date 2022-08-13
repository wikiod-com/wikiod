---
title: "How to reset django migrations"
slug: "how-to-reset-django-migrations"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

As you develop a Django app, there might be situations where you can save a lot of time by just cleaning up and resetting your migrations.

## Resetting Django Migration: Deleting existing database and migrating as fresh
Drop/Delete your database
If you are using SQLite for your database, just delete this file.
If you are using MySQL/Postgres or any other database system, you will have to drop the database and then recreate a fresh database.

You will now need to delete all the migrations file except "init.py" file located inside the migrations folder under your app folder.

Usually the migrations folder is located at

    /your_django_project/your_app/migrations

Now that you have deleted the database and the migrations file, just run the following commands as you would migrate the first time you setup django project.

    python manage.py makemigrations
    python manage.py migrate

