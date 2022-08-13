---
title: "Configuring mysql with sails.js"
slug: "configuring-mysql-with-sailsjs"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## How to configure mysql database connection in sails.js
To do this first locate config folder in your root. Then open `connections.js`

Locate 

      // someMysqlServer: {
      //   adapter: 'sails-mysql',
      //   host: 'YOUR_MYSQL_SERVER_HOSTNAME_OR_IP_ADDRESS',
      //   user: 'YOUR_MYSQL_USER', //optional
      //   password: 'YOUR_MYSQL_PASSWORD', //optional
      //   database: 'YOUR_MYSQL_DB' //optional
      // },

Uncomment these lines.

Give suitable name for the connector like this someMysqlServer to mysql_connection or any name as your wish

      mysql_connection: {
        adapter: 'sails-mysql',
        host: '127.0.0.1', // can user localhost or mysql connection either
        user: 'root', // your mysql username
        password: 'xxxxxxxxx', // your mysql password
        database: 'your database name here' // database name
      },

Save file

Go to your root folder and run following command :

    $ npm install sails-mysql --save

Note: by running above command we are installing mysql driver package for sails.js.

Your are done.

Now you can use mysql_connection as connection name in your model config. When you lift your app using following command:

    $ sails lift

you model schema will automatically get updated in MySQL database
     

