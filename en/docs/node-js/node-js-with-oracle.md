---
title: "Node.js with Oracle"
slug: "nodejs-with-oracle"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Connect to Oracle DB
A very easy way to connect to an ORACLE database is by using [`oracledb`][1] module. This module handles the connection between your Node.js app and Oracle server.
You can install it like any other module:

    npm install oracledb

Now you have to create an ORACLE connection, which you can later query.

    const oracledb = require('oracledb');

    oracledb.getConnection(
      {
        user          : "oli",
        password      : "password",
        connectString : "ORACLE_DEV_DB_TNS_NAME"
      },
      connExecute
    );

The connectString "ORACLE_DEV_DB_TNA_NAME" may live in a tnsnames.org file in the same directory or where your oracle instant client is installed.

If you don't have any oracle instant client installed on you development machine you may follow the [`instant client installation guide`][2] for your operating system.

  [1]: https://github.com/oracle/node-oracledb
  [2]: https://github.com/oracle/node-oracledb/blob/master/INSTALL.md#which-instructions-to-follow


## Using a local module for easier querying
To simplify your querying from ORACLE-DB, you may want to call your query like this:

    const oracle = require('./oracle.js');

    const sql = "select 'test' as c1, 'oracle' as c2 from dual";
    oracle.queryObject(sql, {}, {})
        .then(function(result) {
            console.log(result.rows[0]['C2']);
        })
        .catch(function(err) {
            next(err);
        });

Building up the connection and executing is included in this oracle.js file with content as follows:

    'use strict';
    const oracledb = require('oracledb');

    const oracleDbRelease = function(conn) {
      conn.release(function (err) {
        if (err)
          console.log(err.message);
      });
    };

    function queryArray(sql, bindParams, options) {
        options.isAutoCommit = false; // we only do SELECTs
     
        return new Promise(function(resolve, reject) {
            oracledb.getConnection(
                      {
                        user          : "oli",
                        password      : "password",
                        connectString : "ORACLE_DEV_DB_TNA_NAME"
                    })
            .then(function(connection){
                //console.log("sql log: " + sql + " params " + bindParams);
                connection.execute(sql, bindParams, options)
                .then(function(results) {
                    resolve(results);
                    process.nextTick(function() {
                        oracleDbRelease(connection);
                    });
                })
                .catch(function(err) {
                    reject(err);
     
                    process.nextTick(function() {
                        oracleDbRelease(connection);
                            });
                        });
                })
                .catch(function(err) {
                    reject(err);
                });
        });
    }

    function queryObject(sql, bindParams, options) {
        options['outFormat'] = oracledb.OBJECT; // default is oracledb.ARRAY
        return queryArray(sql, bindParams, options);
    }

    module.exports = queryArray; 
    module.exports.queryArray = queryArray; 
    module.exports.queryObject = queryObject;

Note that you have both methods queryArray and queryObject to call on your oracle object.


## Query a connection object without parameters
Use may now use the connExecute-Function for executing a query. You have the option to get the query result as an object or array. The result ist printed to console.log.

    function connExecute(err, connection)
    {
        if (err) {
            console.error(err.message);
            return;
        }
        sql = "select 'test' as c1, 'oracle' as c2 from dual";
        connection.execute(sql, {}, { outFormat: oracledb.OBJECT }, // or oracledb.ARRAY
            function(err, result)
            {
                if (err) {
                    console.error(err.message);
                    connRelease(connection);
                    return;
                }
                console.log(result.metaData);
                console.log(result.rows);
                connRelease(connection);
            });
    }

Since we used a non-pooling connection, we have to release our connection again.

    function connRelease(connection)
    {
      connection.close(
        function(err) {
          if (err) {
            console.error(err.message);
          }
        });
    }

The output for an object will be

    [ { name: 'C1' }, { name: 'C2' } ]
    [ { C1: 'test', C2: 'oracle' } ]
and the output for an array will be

    [ { name: 'C1' }, { name: 'C2' } ]
    [ [ 'test', 'oracle' ] ]


