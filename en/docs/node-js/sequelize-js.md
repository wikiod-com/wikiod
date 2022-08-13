---
title: "Sequelize.js"
slug: "sequelizejs"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Installation
Make sure that you first have Node.js and npm installed. Then install sequelize.js with npm

    npm install --save sequelize

You will also need to install supported database Node.js modules. You only need to install the one you are using<br><br>
For `MYSQL` and `Mariadb`
 

    npm install --save mysql

For `PostgreSQL`

    npm install --save pg pg-hstore

For `SQLite` 

     npm install --save sqlite
For `MSSQL`

    npm install --save tedious

Once you have you set up installed you can include and create a new Sequalize instance like so.

ES5 syntax

    var Sequelize = require('sequelize');
    var sequelize = new Sequelize('database', 'username', 'password');

ES6 stage-0 Babel syntax

    import Sequelize from 'sequelize';
    const sequelize = new Sequelize('database', 'username', 'password');


You now have an instance of sequelize available. You could if you so feel inclined call it a different name such as 

    var db = new Sequelize('database', 'username', 'password');

or 

    var database = new Sequelize('database', 'username', 'password');

that part is your prerogative. Once you have this installed you can use it inside of your application as per the API documentation http://docs.sequelizejs.com/en/v3/api/sequelize/

Your next step after install would be to [set up your own model](http://docs.sequelizejs.com/en/v3/docs/getting-started/#your-first-model)

## Defining Models
There are two ways to define models in sequelize; with `sequelize.define(...)`, or `sequelize.import(...)`. Both functions return a sequelize model object.

# 1. sequelize.define(modelName, attributes, [options])
This is the way to go if you'd like to define all your models in one file, or if you want to have extra control of your model definition.

```js
/* Initialize Sequelize */
const config = {
    username: "database username",
    password: "database password",
    database: "database name",
    host: "database's host URL",
    dialect: "mysql" // Other options are postgres, sqlite, mariadb and mssql.
}
var Sequelize = require("sequelize");
var sequelize = new Sequelize(config);

/* Define Models */
sequelize.define("MyModel", {
    name: Sequelize.STRING,
    comment: Sequelize.TEXT,
    date: {
        type: Sequelize.DATE,
        allowNull: false
    }
});
```

For the documentation and more examples, check out the [doclets documentation][1], or [sequelize.com's documentation][2].

---

# 2. sequelize.import(path)

If your model definitions are broken into a file for each, then `import` is your friend. In the file where you initialize Sequelize, you need to call import like so:

```js
/* Initialize Sequelize */
// Check previous code snippet for initialization

/* Define Models */
sequelize.import("./models/my_model.js"); // The path could be relative or absolute
```

Then in your model definition files, your code will look something like this:

```js
module.exports = function(sequelize, DataTypes) {
    return sequelize.define("MyModel", {
        name: DataTypes.STRING,
        comment: DataTypes.TEXT,
        date: {
            type: DataTypes.DATE,
            allowNull: false
        }
    });
};
```

For more information on how to use `import`, check out sequelize's [express example on GitHub][3].


  [1]: https://doclets.io/sequelize/sequelize/master#dl-Sequelize-define
  [2]: http://docs.sequelizejs.com/en/v3/docs/models-definition/
  [3]: https://github.com/sequelize/express-example/tree/master/models

