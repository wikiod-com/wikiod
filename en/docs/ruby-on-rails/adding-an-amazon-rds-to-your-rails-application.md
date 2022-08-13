---
title: "Adding an Amazon RDS to your rails application"
slug: "adding-an-amazon-rds-to-your-rails-application"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Steps to create an AWS RDS instance and configure your database.yml file by installing the required connectors.

## Consider we are connecting MYSQL RDS with your rails application.
**Steps to create MYSQL database**
1. Login to amazon account and select RDS service
2. Select `Launch DB Instance` from the instance tab
3. By defaul MYSQL Community Edition will be selected, hence click the `select` button
4. Select the database purpose, say `production` and click `next step`
5. Provide the `mysql version, storage size, DB Instance Identifier, Master Username and Password` and click `next step`
6. Enter `Database Name` and click `Launch DB Instance`
7. Please wait until all the instance gets created. Once the instance gets created you will find an Endpoint, copy this entry point (which is referred as hostname)

**Installing connectors**

Add the MySQL database adapter to your project's gemfile,

    gem 'mysql2'
Install your added gems,

    bundle install
Some other database adapters are,
* `gem 'pg'` for PostgreSQL
* `gem 'activerecord-oracle_enhanced-adapter'` for Oracle
* `gem 'sql_server'` for SQL Server

**Configure your project's database.yml file**
Open your config/database.yml file 

    production:
      adapter: mysql2
      encoding: utf8
      database: <%= RDS_DB_NAME %>  # Which you have entered you creating database
      username: <%= RDS_USERNAME %> # db master username
      password: <%= RDS_PASSWORD %> # db master password
      host: <%= RDS_HOSTNAME %>     # db instance entrypoint
      port: <%= RDS_PORT %>         # db post. For MYSQL 3306

