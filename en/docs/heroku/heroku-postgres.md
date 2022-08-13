---
title: "Heroku Postgres"
slug: "heroku-postgres"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## How to Reset Postgres Database in Heroku
Steps to reset database in Heroku:

**1. Drop the database, when SHARED_DATABASE_URL is used:**

    heroku pg:reset DATABASE

**2. Recreate the database with nothing in it:**

    heroku run rake db:migrate

**3. Populate the database with your seed data:**

    heroku run rake db:seed

**Steps 2 and 3 can be combined into one command by executing this:**

    heroku run rake db:setup

## How to copy heroku database to local database
Steps to copy heroku database to local database:

**1. Run copy process in terminal:**  

<code>heroku pg:pull DATABASE_URL change_to_your_data_base_name --app change_to_your_app_name</code>

**2. Change db owner using this query:**  

<code>GRANT ALL PRIVILEGES ON DATABASE change_to_your_data_base_name to change_to_your_user;
ALTER DATABASE change_to_your_data_base_name OWNER TO change_to_your_user; </code>

**3. Generate and run query for all tables in you database:**  

<code>SELECT 'ALTER TABLE '|| schemaname || '.' || tablename ||' OWNER TO change_to_your_user;'
FROM pg_tables WHERE NOT schemaname IN ('pg_catalog', 'information_schema')
ORDER BY schemaname, tablename;</code>

